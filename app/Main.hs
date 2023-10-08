{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Backup.ExternalDisk qualified as ExternalDiskBackup
import Backup.PeriodicBackup qualified as PeriodicBackup
import Backup.RSync qualified as RSync
import Cli qualified
import Config.Backup.ExternalDisk (ExternalDiskBackupConfig (..))
import Config.Backup.PeriodicBackup (PeriodicBackupConfig (..))
import Config.Drive (MountDriveConfig (..))
import Config.GetConfig qualified as Config
import Config.TopLevel qualified as TopLevel
import Control.Monad (forM, forM_, unless)
import Data.Functor (void)
import Display (display)
import Drive.MountDrive (blockUntilDiskAvailable, closeDisk, tryMounting)
import Effectful (Eff)
import Effectful.Concurrent.Async qualified as Concurrent
import Effectful.Reader.Static qualified as Reader
import Logger qualified
import RunEffects (TopLevelEffects, runEffects)
import State.MostRecentBackup qualified as MostRecentBackup

main :: IO ()
main = runEffects $ do
  commandLineArguments <- Cli.getCommandLineArguments

  config <- Config.readConfig

  case commandLineArguments of
    Cli.Run -> run config
    Cli.Mount -> mount config
    Cli.Unmount -> unmount config

-- | Runs the application in continuous backup mode
run :: TopLevel.Config -> Eff TopLevelEffects ()
run config = do
  Logger.displayInfo config
  Logger.logInfo ""

  -- Mount any configured drives
  forM_ config.mountBackupDrive $ \backupDriveConfig -> do
    Logger.logTrace "Waiting for backup drive to become available"
    Reader.runReader backupDriveConfig $ do
      -- Wait until it is available
      blockUntilDiskAvailable

  Logger.logTrace "Migrating database tables"
  -- Create the database and tables
  MostRecentBackup.migrateTables config.state

  Logger.logTrace "Startup complete"

  -- Run periodic rsync backups
  rsyncAsyncs <- forM config.rsyncBackups $ \periodicRsyncBackupConfig ->
    Concurrent.async $
      PeriodicBackup.loopPeriodicBackup config.state periodicRsyncBackupConfig $
        RSync.run periodicRsyncBackupConfig.backup

  -- Loop on all external disk backups
  externalDiskAsyncs <- forM config.externalDiskBackups $ \externalDiskBackup ->
    Concurrent.async $ ExternalDiskBackup.loop externalDiskBackup

  let
    allAsyncs = rsyncAsyncs ++ externalDiskAsyncs

  unless (null allAsyncs) $
    void $
      Concurrent.waitAnyCancel allAsyncs

-- | Mount all drives mentioned in the config and exit
mount :: TopLevel.Config -> Eff TopLevelEffects ()
mount config = do
  forM_ config.mountBackupDrive $ \backupDriveConfig -> do
    Logger.displayTrace $ "Mounting " <> display backupDriveConfig.driveName
    Reader.runReader backupDriveConfig $ do
      tryMounting

  forM_ config.externalDiskBackups $ \externalDiskBackup -> do
    Logger.displayTrace $ "Mounting " <> display externalDiskBackup.mountConfig.driveName
    Reader.runReader externalDiskBackup.mountConfig $ do
      tryMounting

-- | Unmount all drives mentioned in the config and exit
unmount :: TopLevel.Config -> Eff TopLevelEffects ()
unmount config = do
  forM_ config.mountBackupDrive $ \backupDriveConfig ->
    Reader.runReader backupDriveConfig $ do
      Logger.displayTrace $ "Unmounting " <> display backupDriveConfig.driveName
      closeDisk

  forM_ config.externalDiskBackups $ \externalDiskBackup -> do
    Logger.displayTrace $ "Unmounting " <> display externalDiskBackup.mountConfig.driveName
    Reader.runReader externalDiskBackup.mountConfig $ do
      closeDisk
