{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Effect to manage the mounting of a drive
module Drive.MountDrive (
  MountDrive,
  isDriveConnected,
  isDriveMounted,
  luksOpen,
  luksClose,
  mount,
  unmount,
  fsckRepair,
  formatExfat,
  runMountDrive,
  tryMounting,
  blockUntilDiskAvailable,
  blockUntilDiskGone,
  closeDisk,
  driveUuidMapperPath,
) where

import Command (Command, CommandError)
import Command qualified
import Config.Drive (FsckMode (..), MountDriveConfig (..), MountingMode (..))
import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as Text
import Display (Display (..))
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as Concurrent
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem qualified as FileSystem
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Logger (Logger, displayError)
import Secrets (Secrets)
import Secrets qualified
import System.Exit (ExitCode (..))

data MountDrive :: Effect where
  IsDriveConnected :: MountDriveConfig -> MountDrive m Bool
  IsDriveMounted :: MountDriveConfig -> MountDrive m Bool
  LuksOpen :: MountDriveConfig -> MountDrive m ()
  LuksClose :: MountDriveConfig -> MountDrive m ()
  Mount :: MountDriveConfig -> MountDrive m ()
  Unmount :: MountDriveConfig -> MountDrive m ()
  FsckRepair :: MountDriveConfig -> MountDrive m ()
  FormatExfat :: MountDriveConfig -> MountDrive m ()

type instance DispatchOf MountDrive = Dynamic

luksOpen :: (Error.HasCallStack, MountDrive :> es, Reader MountDriveConfig :> es) => Eff es ()
luksOpen = Reader.ask @MountDriveConfig >>= send . LuksOpen

luksClose :: (Error.HasCallStack, MountDrive :> es, Reader MountDriveConfig :> es) => Eff es ()
luksClose = Reader.ask @MountDriveConfig >>= send . LuksClose

unmount :: (Error.HasCallStack, MountDrive :> es, Reader MountDriveConfig :> es) => Eff es ()
unmount = Reader.ask @MountDriveConfig >>= send . Unmount

mount :: (Error.HasCallStack, MountDrive :> es, Reader MountDriveConfig :> es) => Eff es ()
mount = Reader.ask @MountDriveConfig >>= send . Mount

formatExfat :: (Error.HasCallStack, MountDrive :> es, Reader MountDriveConfig :> es) => Eff es ()
formatExfat = Reader.ask @MountDriveConfig >>= send . FormatExfat

isDriveMounted
  :: (Error.HasCallStack, MountDrive :> es, Reader MountDriveConfig :> es) => Eff es Bool
isDriveMounted = Reader.ask @MountDriveConfig >>= send . IsDriveMounted

isDriveConnected
  :: (Error.HasCallStack, MountDrive :> es, Reader MountDriveConfig :> es) => Eff es Bool
isDriveConnected = Reader.ask @MountDriveConfig >>= send . IsDriveConnected

fsckRepair
  :: (Error.HasCallStack, MountDrive :> es, Reader MountDriveConfig :> es) => Eff es ()
fsckRepair = Reader.ask @MountDriveConfig >>= send . FsckRepair

runMountDrive
  :: ( Error.HasCallStack
     , IOE :> es
     , Secrets :> es
     , Command :> es
     , Logger :> es
     , Error CommandError :> es
     )
  => Eff (MountDrive : es) a
  -> Eff es a
runMountDrive = reinterpret FileSystem.runFileSystem $ \_ -> \case
  IsDriveConnected config ->
    FileSystem.doesPathExist config.drivePath
  IsDriveMounted config -> do
    (exitCode, _stdout, _stderr) <-
      Command.readProcessWithExitCode "findmnt" [config.mountDirectory] ""

    case exitCode of
      ExitSuccess -> pure True
      _ -> pure False
  LuksOpen config ->
    case config.mountingMode of
      MountWithoutEncryption -> pure ()
      MountWithPartitionLuks -> do
        encryptionSecret <- Secrets.getSecret "DISK_ENCRYPTION_SECRET"
        Command.runSudoProcessThrowOnError
          "cryptsetup"
          [ "open"
          , config.drivePath
          , Text.unpack config.driveName
          ]
          (Text.unpack $ encryptionSecret.value)
  LuksClose config ->
    case config.mountingMode of
      MountWithoutEncryption -> pure ()
      MountWithPartitionLuks -> do
        Command.runSudoProcessThrowOnError
          "cryptsetup"
          [ "close"
          , driveUuidMapperPath config.driveName
          ]
          ""
  Mount config ->
    Command.runSudoProcessThrowOnError
      "mount"
      [ "--mkdir"
      , partitionPath config
      , config.mountDirectory
      ]
      ""
  Unmount config ->
    Command.runSudoProcessThrowOnError
      "umount"
      [partitionPath config]
      ""
  FsckRepair config ->
    case config.fsck of
      DoNotFsck -> pure ()
      FsckRepairExfat -> do
        (exitCode, stdout, stderr) <-
          Command.runSudoProcess
            "fsck.exfat"
            ["--repair-yes", config.drivePath]
            ""

        case exitCode of
          ExitSuccess -> pure ()
          ExitFailure code -> do
            displayError $
              "fsck.exfat errored with code "
                <> display code
                <> "\nstdout:\n"
                <> display stdout
                <> "\nstderr:\n"
                <> display stderr
      FsckBtrfsCheckNoRepair -> do
        Command.runSudoProcessThrowOnError "btrfs" ["check", partitionPath config] ""
  FormatExfat config ->
    Command.runSudoProcessThrowOnError
      "mkfs.exfat"
      [ "--volume-label=" <> Text.unpack config.driveName
      , config.drivePath
      ]
      ""

tryMounting :: (Error.HasCallStack, Reader MountDriveConfig :> es, MountDrive :> es) => Eff es ()
tryMounting = do
  connected <- isDriveConnected
  when connected $ do
    mounted <- isDriveMounted
    unless mounted $ do
      luksOpen
      _exitCode <- fsckRepair
      mount

-- | Waits until the disk is connected. When it is, it mounts it, and returns when the drive is
-- successfully mounted
blockUntilDiskAvailable
  :: (Error.HasCallStack, Reader MountDriveConfig :> es, Concurrent :> es, MountDrive :> es)
  => Eff es ()
blockUntilDiskAvailable = do
  loopConnected
  tryMounting
 where
  loopConnected = do
    connected <- isDriveConnected
    unless connected $ do
      config <- Reader.ask @MountDriveConfig
      Concurrent.threadDelay $ config.pollDelayMs * 1000
      loopConnected

-- | Unmount and luks close the disk
closeDisk :: (Error.HasCallStack, Reader MountDriveConfig :> es, MountDrive :> es) => Eff es ()
closeDisk = do
  connected <- isDriveConnected
  mounted <- isDriveMounted
  -- After some error cases, a drive can be mounted, but not actually connected! This will unmount
  -- the directory.
  when mounted $
    unmount
  when connected $
    luksClose

-- | Waits until the drive is disconnected, i.e. no longer visible to the system.
blockUntilDiskGone
  :: (Error.HasCallStack, Reader MountDriveConfig :> es, Concurrent :> es, MountDrive :> es)
  => Eff es ()
blockUntilDiskGone = do
  connected <- isDriveConnected
  when connected $ do
    config <- Reader.ask @MountDriveConfig
    Concurrent.threadDelay $ config.pollDelayMs * 1000
    blockUntilDiskGone

driveUuidMapperPath :: Text -> FilePath
driveUuidMapperPath name =
  "/dev/mapper/" ++ Text.unpack name

-- | The path of the partition, takes into account whether the drive is encrypted.
partitionPath :: MountDriveConfig -> FilePath
partitionPath config =
  case config.mountingMode of
    MountWithoutEncryption -> config.drivePath
    MountWithPartitionLuks -> driveUuidMapperPath config.driveName
