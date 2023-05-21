{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backup.ExternalDisk (ExternalDiskBackup, run, runExternalDiskBackup, loop) where

import Backup.RSync (RSync)
import Backup.RSync qualified as RSync
import Command (Command, CommandError)
import Command qualified
import Config.Backup.ExternalDisk (ExternalDiskBackupConfig (..), FormatOption (..))
import Config.Backup.Rsync (RsyncBackupConfig (..))
import Config.Drive (MountDriveConfig (..))
import Display (Display (..))
import Drive.MountDrive (MountDrive)
import Drive.MountDrive qualified as MountDrive
import Effectful (Eff, Effect, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static qualified as Reader
import Effectful.TH (makeEffect)
import Logger (Logger)
import Logger qualified

-- | Waits for the a disk to appear, mounts it, backs up a directory, and then unmounts the disk
-- again
data ExternalDiskBackup :: Effect where
  Run :: ExternalDiskBackupConfig -> ExternalDiskBackup m ()

makeEffect ''ExternalDiskBackup

-- | Represents the steps taken in the backup, used for logging
data TraceSteps
  = WaitingForDisk ExternalDiskBackupConfig
  | StartingBackup ExternalDiskBackupConfig
  | BackupComplete ExternalDiskBackupConfig
  | UnmountComplete ExternalDiskBackupConfig
  | FormattingDrive ExternalDiskBackupConfig
  | FormattingComplete ExternalDiskBackupConfig
  | WaitUntilDiskIsGone ExternalDiskBackupConfig

instance Display TraceSteps where
  display = \case
    WaitingForDisk config ->
      header config
        <> ": Waiting for disk "
        <> display config.mountConfig.driveUuid
        <> " to appear"
    StartingBackup config ->
      header config
        <> ": Disk "
        <> display config.mountConfig.driveUuid
        <> " is available, starting backup."
    BackupComplete config ->
      header config
        <> ": backup finished, unmounting disk."
    UnmountComplete config ->
      header config
        <> ": Unmount complete."
    FormattingDrive config ->
      header config
        <> ": Formatting drive"
    FormattingComplete config ->
      header config
        <> ": Formatting complete"
    WaitUntilDiskIsGone config ->
      header config
        <> ": Waiting until disk has disappeared"
   where
    header config = "Backup " <> display config.rsyncConfig.backupName


runExternalDiskBackup
  :: (MountDrive :> es, RSync :> es, Concurrent :> es, Logger :> es, Command :> es, Error CommandError :> es)
  => Eff (ExternalDiskBackup : es) a
  -> Eff es a
runExternalDiskBackup = interpret $ \_ -> \case
  Run config -> do
    Reader.runReader config.mountConfig $ do
      Logger.displayTrace $ WaitingForDisk config
      MountDrive.blockUntilDiskAvailable

      Logger.displayTrace $ StartingBackup config
      RSync.run config.rsyncConfig
      Logger.displayTrace $ BackupComplete config

      MountDrive.unmount
      Logger.displayTrace $ UnmountComplete config

      case config.formatAfterBackup of
        DoNotFormat -> pure ()
        FormatExfat -> do
          Logger.displayTrace $ FormattingDrive config
          Command.runProcessThrowOnError
            "sudo"
            [ "mkfs.exfat"
            , MountDrive.driveUuidDiskPath config.mountConfig.driveUuid
            ]
            ""
          Logger.displayTrace $ FormattingComplete config

-- | Waits for a disk to appear, mounts it, backs up a directory, unmounts the disk again, waits
-- until the disk is gone, and then repeats the process
loop
  :: (ExternalDiskBackup :> es, MountDrive :> es, Logger :> es, Concurrent :> es)
  => ExternalDiskBackupConfig
  -> Eff es ()
loop config =
  go
 where
  go = do
    run config
    Logger.displayTrace $ WaitUntilDiskIsGone config
    Reader.runReader config.mountConfig MountDrive.blockUntilDiskGone

    go
