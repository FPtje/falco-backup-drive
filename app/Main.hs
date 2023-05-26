{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Backup.ExternalDisk qualified as ExternalDiskBackup
import Backup.PeriodicBackup qualified as PeriodicBackup
import Backup.RSync qualified as RSync
import Config.Backup.PeriodicBackup (PeriodicBackupConfig (..))
import Config.GetConfig qualified as Config
import Config.TopLevel qualified as TopLevel
import Control.Monad (forM, forM_, unless)
import Data.Functor (void)
import Drive.MountDrive (
  blockUntilDiskAvailable,
 )
import Effectful.Concurrent.Async qualified as Concurrent
import Effectful.Reader.Static qualified as Reader
import Logger qualified
import RunEffects (runEffects)
import State.MostRecentBackup qualified as MostRecentBackup

main :: IO ()
main = runEffects $ do
  config <- Config.readConfig

  Logger.displayInfo config
  Logger.logInfo ""

  -- Create the database and tables
  MostRecentBackup.migrateTables config.state

  -- Mount any configured drives
  forM_ config.mountBackupDrive $ \backupDriveConfig ->
    Reader.runReader backupDriveConfig blockUntilDiskAvailable

  -- Run perioic rsync backups
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
