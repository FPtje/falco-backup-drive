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
import Data.Text qualified as Text
import Display (Display (..))
import Drive.MountDrive (MountDrive)
import Drive.MountDrive qualified as MountDrive
import Effectful (Eff, Effect, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error qualified as Error
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
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
data Trace = Trace ExternalDiskBackupConfig TraceStep

-- | An individual traced step
data TraceStep
  = WaitingForDisk
  | StartingBackup
  | BackupComplete
  | UnmountComplete
  | FormattingDrive
  | FormattingComplete
  | BackupFailed Error.CallStack CommandError
  | WaitUntilDiskIsGone

instance Display Trace where
  display (Trace config step) =
    "Backup "
      <> display config.rsyncConfig.backupName
      <> ": "
      <> displayStep
   where
    displayStep = case step of
      WaitingForDisk ->
        "Waiting for disk "
          <> display config.mountConfig.driveName
          <> " to appear"
      StartingBackup ->
        "Disk "
          <> display config.mountConfig.driveName
          <> " is available, starting backup."
      BackupComplete ->
        "backup finished, unmounting disk."
      UnmountComplete ->
        "Unmount complete."
      FormattingDrive ->
        "Formatting drive"
      FormattingComplete ->
        "Formatting complete"
      BackupFailed callstack err ->
        "Backup failed with the following error:\n" <> Error.displayError (callstack, err)
      WaitUntilDiskIsGone ->
        "Waiting until disk has disappeared"

runExternalDiskBackup
  :: (MountDrive :> es, RSync :> es, Concurrent :> es, Logger :> es, Command :> es, Error CommandError :> es)
  => Eff (ExternalDiskBackup : es) a
  -> Eff es a
runExternalDiskBackup = interpret $ \_ -> \case
  Run config -> do
    Reader.runReader config.mountConfig $ do
      trace config WaitingForDisk
      MountDrive.blockUntilDiskAvailable

      trace config StartingBackup
      RSync.run config.rsyncConfig
      trace config BackupComplete

      MountDrive.closeDisk
      trace config UnmountComplete

      case config.formatAfterBackup of
        DoNotFormat -> pure ()
        FormatExfat -> do
          trace config FormattingDrive
          Command.runSudoProcessThrowOnError
            "mkfs.exfat"
            [ "--volume-label=" <> Text.unpack config.mountConfig.driveName
            , config.mountConfig.drivePath
            ]
            ""
          trace config FormattingComplete

-- | Waits for a disk to appear, mounts it, backs up a directory, unmounts the disk again, waits
-- until the disk is gone, and then repeats the process
loop
  :: (ExternalDiskBackup :> es, MountDrive :> es, Logger :> es, Concurrent :> es, Error CommandError :> es)
  => ExternalDiskBackupConfig
  -> Eff es ()
loop config =
  go
 where
  go = do
    Error.handleError
      (\callstack err -> trace config $ BackupFailed callstack err)
      $ run config

    trace config WaitUntilDiskIsGone
    Reader.runReader config.mountConfig MountDrive.blockUntilDiskGone

    go

-- | Internal function to trace a step
trace :: Logger :> es => ExternalDiskBackupConfig -> TraceStep -> Eff es ()
trace config step = Logger.displayTrace $ Trace config step
