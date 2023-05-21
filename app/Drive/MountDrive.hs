{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Effect to manage the mounting of a drive
module Drive.MountDrive (
  MountDrive,
  isDriveConnected,
  isDriveMounted,
  mount,
  unmount,
  runMountDrive,
  tryMounting,
  blockUntilDiskAvailable,
  blockUntilDiskGone,
) where

import Command (Command, CommandError)
import Command qualified
import Config.Drive (MountDriveConfig (..), MountingMode (..))
import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as Concurrent
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem qualified as FileSystem
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.TH (makeEffect)
import Secrets (Secrets)
import Secrets qualified
import System.Exit (ExitCode (..))

data MountDrive :: Effect where
  IsDriveConnected :: MountDrive m Bool
  IsDriveMounted :: MountDrive m Bool
  Mount :: MountDrive m ()
  Unmount :: MountDrive m ()

makeEffect ''MountDrive

runMountDrive
  :: (Error.HasCallStack, IOE :> es, Reader MountDriveConfig :> es, Secrets :> es, Command :> es, Error CommandError :> es)
  => Eff (MountDrive : es) a
  -> Eff es a
runMountDrive = reinterpret FileSystem.runFileSystem $ \_ -> \case
  IsDriveConnected -> do
    config <- Reader.ask @MountDriveConfig
    FileSystem.doesPathExist $ driveUuidDiskPath config.driveUuid
  IsDriveMounted -> do
    config <- Reader.ask @MountDriveConfig
    (exitCode, _stdout, _stderr) <-
      Command.readProcessWithExitCode "findmnt" [config.mountDirectory] ""

    case exitCode of
      ExitSuccess -> pure True
      _ -> pure False
  Mount -> do
    config <- Reader.ask @MountDriveConfig
    case config.mountingMode of
      MountWithoutEncryption -> do
        Command.runProcessThrowOnError
          "sudo"
          [ "mount"
          , "--mkdir"
          , driveUuidDiskPath config.driveUuid
          , config.mountDirectory
          ]
          ""
      MountWithPartitionLuks -> do
        encryptionSecret <- Secrets.getSecret "DISK_ENCRYPTION_SECRET"
        Command.runProcessThrowOnError
          "sudo"
          [ "cryptsetup"
          , "open"
          , driveUuidDiskPath config.driveUuid
          , Text.unpack config.driveUuid
          ]
          (Text.unpack $ encryptionSecret.value)

        Command.runProcessThrowOnError
          "sudo"
          [ "mount"
          , "--mkdir"
          , driveUuidMapperPath config.driveUuid
          , config.mountDirectory
          ]
          ""
  Unmount -> do
    config <- Reader.ask @MountDriveConfig
    case config.mountingMode of
      MountWithoutEncryption -> do
        Command.runProcessThrowOnError
          "sudo"
          [ "umount"
          , driveUuidDiskPath config.driveUuid
          ]
          ""
      MountWithPartitionLuks -> do
        Command.runProcessThrowOnError
          "sudo"
          [ "umount"
          , driveUuidMapperPath config.driveUuid
          ]
          ""

        Command.runProcessThrowOnError
          "sudo"
          [ "cryptsetup"
          , "close"
          , driveUuidMapperPath config.driveUuid
          ]
          ""

tryMounting :: MountDrive :> es => Eff es ()
tryMounting = do
  connected <- isDriveConnected
  when connected $ do
    mounted <- isDriveMounted
    unless mounted mount

-- | Waits until the disk is connected. When it is, it mounts it, and returns when the drive is
-- successfully mounted
blockUntilDiskAvailable
  :: (Reader MountDriveConfig :> es, Concurrent :> es, MountDrive :> es)
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

-- | Waits until the drive is disconnected, i.e. no longer visible to the system.
blockUntilDiskGone
  :: (Reader MountDriveConfig :> es, Concurrent :> es, MountDrive :> es)
  => Eff es ()
blockUntilDiskGone = do
  connected <- isDriveConnected
  when connected $ do
    config <- Reader.ask @MountDriveConfig
    Concurrent.threadDelay $ config.pollDelayMs * 1000
    blockUntilDiskGone

driveUuidDiskPath :: Text -> FilePath
driveUuidDiskPath uuid =
  "/dev/disk/by-uuid/" ++ Text.unpack uuid

driveUuidMapperPath :: Text -> FilePath
driveUuidMapperPath uuid =
  "/dev/mapper/" ++ Text.unpack uuid
