{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Backup.RSync qualified as RSync
import Command (CommandError)
import Command qualified
import Config.GetConfig qualified as Config
import Config.TopLevel qualified as TopLevel
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (forM_)
import Display (Display)
import Drive.MountDrive (
  blockUntilDiskAvailable,
  runMountDrive,
 )
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent qualified as Concurrent
import Effectful.Environment qualified as Environment
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader
import Logger (Logger)
import Logger qualified
import Secrets qualified
import System.Exit (exitFailure)

main :: IO ()
main = do
  runEff $ Logger.logStdout $ Config.runGetConfig $ Environment.runEnvironment $ do
    config <- Config.readConfig

    Logger.displayInfo config
    Concurrent.runConcurrent $
      runFailOnError @CommandError $
        runFailOnError @Secrets.SecretError $
          Command.runCommand $
            Secrets.runSecrets $ do
              forM_ config.mountBackupDrive $ \backupDriveConfig ->
                Reader.runReader backupDriveConfig $
                  runMountDrive blockUntilDiskAvailable

              forM_ config.rsyncBackups $ \rsyncBackupConfig ->
                Reader.runReader rsyncBackupConfig $ RSync.runRSync RSync.run

-- | Run an effect, and on failure, print the error and exit with failure
runFailOnError
  :: forall e es a
   . (IOE :> es, Logger :> es, Display e)
  => Eff (Error e : es) a
  -> Eff es a
runFailOnError eff = do
  eError <- Error.runError @e eff
  case eError of
    Left (callstack, err) -> do
      Logger.displayInfo $ Error.prettyCallStack callstack
      Logger.displayInfo err
      liftIO exitFailure
    Right res -> pure res
