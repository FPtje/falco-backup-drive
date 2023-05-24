{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Backup.ExternalDisk qualified as ExternalDiskBackup
import Backup.RSync qualified as RSync
import Command (CommandError)
import Command qualified
import Config.GetConfig qualified as Config
import Config.TopLevel qualified as TopLevel
import Control.Monad (forM, forM_, unless)
import Data.Functor (void)
import Database.Persistent.SqliteEffect qualified as Sqlite
import Drive.MountDrive (
  blockUntilDiskAvailable,
  runMountDrive,
 )
import Effectful (runEff)
import Effectful.Concurrent qualified as Concurrent
import Effectful.Concurrent.Async qualified as Concurrent
import Effectful.Environment qualified as Environment
import Effectful.Error qualified as Error
import Effectful.Reader.Static qualified as Reader
import Logger qualified
import Secrets qualified
import State.MostRecentBackup qualified as MostRecentBackup

main :: IO ()
main = do
  runEff $ Logger.logStdout $ Config.runGetConfig $ Environment.runEnvironment $ do
    config <- Config.readConfig

    Logger.displayInfo config

    Logger.logInfo ""

{- FOURMOLU_DISABLE -}
    Concurrent.runConcurrent $ Error.runFailOnError @CommandError $
      Error.runFailOnError @Secrets.SecretError $ Command.runCommand $ Sqlite.runSqliteIO $
      Secrets.runSecrets $ runMountDrive $ RSync.runRSync $
      MostRecentBackup.runMostRecentBackupStateSqlite config.state $
      ExternalDiskBackup.runExternalDiskBackup $ do
        -- Create the database and tables
        MostRecentBackup.migrateTables config.state

        -- Mount any configured drives
        forM_ config.mountBackupDrive $ \backupDriveConfig ->
            Reader.runReader backupDriveConfig blockUntilDiskAvailable

        -- Run any one-off rsync backups (TODO: have them repeat on a schedule)
        forM_ config.rsyncBackups $ \rsyncBackupConfig ->
          RSync.run rsyncBackupConfig

        -- Loop on all external disk backups
        asyncs <- forM config.externalDiskBackups $ \externalDiskBackup ->
          Concurrent.async $ ExternalDiskBackup.loop externalDiskBackup

        unless (null asyncs) $
          void $ Concurrent.waitAnyCancel asyncs
{- FOURMOLU_ENABLE -}
