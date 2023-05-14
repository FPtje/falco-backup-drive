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
  MountDriveConfig (..),
  CommandInfo (..),
  MountingError (..),
  MountingMode (..),
  isDriveConnected,
  isDriveMounted,
  mountDrive,
  runMountDrive,
  tryMounting,
  blockUntilDiskAvailable,
) where

import Config.Drive (MountDriveConfig (..), MountingMode (..))
import Control.Monad (unless, when)
import Control.Monad.Catch (catchIOError)
import Data.Text (Text)
import Data.Text qualified as Text
import Display (Display (..))
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as Concurrent
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem qualified as FileSystem
import Effectful.Process (Process)
import Effectful.Process qualified as Process
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.TH (makeEffect)
import System.Exit (ExitCode (..))

data MountDrive :: Effect where
  IsDriveConnected :: MountDrive m Bool
  IsDriveMounted :: MountDrive m Bool
  MountDrive :: MountDrive m ()

data CommandInfo = CommandInfo
  { command :: String
  , args :: [String]
  , stdout :: String
  , stderr :: String
  }

instance Display CommandInfo where
  display cmd =
    display cmd.command
      <> " "
      <> display (unwords cmd.args)
      <> "\n\nstdout: "
      <> display cmd.stdout
      <> "\n\nstderr: "
      <> display cmd.stderr

data MountingError
  = MountCommandFailed CommandInfo
  | UnknownCommandError String CommandInfo

instance Display MountingError where
  display = \case
    MountCommandFailed cmdInfo -> "Mounting command failed! Command was: " <> display cmdInfo
    UnknownCommandError err cmdInfo ->
      "Unknown error running command:\n\n"
        <> display err
        <> "\n\nCommand was: "
        <> display cmdInfo

makeEffect ''MountDrive

runMountDrive
  :: (IOE :> es, Reader MountDriveConfig :> es, Error MountingError :> es)
  => Eff (MountDrive : es) a
  -> Eff es a
runMountDrive = reinterpret (FileSystem.runFileSystem . Process.runProcess) $ \_ -> \case
  IsDriveConnected -> do
    config <- Reader.ask @MountDriveConfig
    FileSystem.doesPathExist $ driveUuidDiskPath config.driveUuid
  IsDriveMounted -> do
    config <- Reader.ask @MountDriveConfig
    (exitCode, _stdout, _stderr) <-
      Process.readProcessWithExitCode "findmnt" [config.mountDirectory] ""

    case exitCode of
      ExitSuccess -> pure True
      _ -> pure False
  MountDrive -> do
    config <- Reader.ask @MountDriveConfig
    case config.mountingMode of
      MountWithoutEncryption -> do
        runProcessThrowOnError
          "sudo"
          [ "mount"
          , "--mkdir"
          , driveUuidDiskPath config.driveUuid
          , config.mountDirectory
          ]
          ""
      MountWithPartitionLuks passphrase -> do
        runProcessThrowOnError
          "sudo"
          [ "cryptsetup"
          , "open"
          , driveUuidDiskPath config.driveUuid
          , Text.unpack config.driveUuid
          ]
          (Text.unpack passphrase)

        runProcessThrowOnError
          "sudo"
          [ "mount"
          , "--mkdir"
          , driveUuidMapperPath config.driveUuid
          , config.mountDirectory
          ]
          ""

tryMounting :: MountDrive :> es => Eff es ()
tryMounting = do
  connected <- isDriveConnected
  when connected $ do
    mounted <- isDriveMounted
    unless mounted $ do
      mountDrive

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

runProcessThrowOnError
  :: (Process :> es, Error MountingError :> es)
  => String
  -> [String]
  -> String
  -> Eff es ()
runProcessThrowOnError executable args stdin = do
  (exitCode, stdout, stderr) <-
    Process.readProcessWithExitCode executable args stdin
      `catchIOError` \err ->
        Error.throwError $
          UnknownCommandError (show err) $
            CommandInfo executable args "" ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _code ->
      Error.throwError $ MountCommandFailed $ CommandInfo executable args stdout stderr

driveUuidDiskPath :: Text -> FilePath
driveUuidDiskPath uuid =
  "/dev/disk/by-uuid/" ++ Text.unpack uuid

driveUuidMapperPath :: Text -> FilePath
driveUuidMapperPath uuid =
  "/dev/mapper/" ++ Text.unpack uuid
