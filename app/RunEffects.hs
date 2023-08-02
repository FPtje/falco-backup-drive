{-# LANGUAGE DataKinds #-}

-- | Contains a functions that run all the effects. This function is placed in a separate module due
-- to the use of the PartialTypeSignatures language extension. Enabling that language extension
-- makes it impossible to use type holes for programming, but it also helps prevent writing things
-- down twice. By using it only in a single module, the downside is minimized to only the functions
-- that use it.
module RunEffects where

import Backup.ExternalDisk qualified as ExternalDiskBackup
import Backup.PeriodicBackup qualified as PeriodicBackup
import Backup.RSync qualified as RSync
import Command (CommandError)
import Command qualified
import Config.GetConfig qualified as Config
import Drive.MountDrive qualified as MountDrive
import Effectful (Eff, inject, runEff)
import Effectful.Concurrent qualified as Concurrent
import Effectful.Environment qualified as Environment
import Effectful.Error qualified as Error
import Effectful.Error.Static (Error)
import Effectful.Persistent.SqliteEffect qualified as Sqlite
import Effectful.Time qualified as Time
import Logger qualified
import Secrets qualified
import State.MostRecentBackup qualified as MostRecentBackup

-- | Runs all the effects in use by this program. The _listOfAllEffects is a type hole, because it
-- would be annoying to maintain the type level list as well as the definition. Since figuring out
-- this list is a pretty easy job for the compiler, let's trust it to do just that.
--
-- This function is also useful for repl use.
runEffects
  :: Eff
      [ PeriodicBackup.PeriodicBackup
      , Time.Time
      , ExternalDiskBackup.ExternalDiskBackup
      , MostRecentBackup.MostRecentBackupState
      , RSync.RSync
      , MountDrive.MountDrive
      , Secrets.Secrets
      , Sqlite.Sqlite
      , Command.Command
      , Error Secrets.SecretError
      , Error CommandError
      , Concurrent.Concurrent
      , Environment.Environment
      , Config.GetConfig
      , Logger.Logger
      -- , IOE -- hidden by 'inject'
      ]
      a
  -> IO a
runEffects =
  runEff
    . Logger.logStdout
    . Config.runGetConfig
    . Environment.runEnvironment
    . Concurrent.runConcurrent
    . Error.runFailOnError @CommandError
    . Error.runFailOnError @Secrets.SecretError
    . Command.runCommand
    . Sqlite.runSqliteIO
    . Secrets.runSecrets
    . MountDrive.runMountDrive
    . RSync.runRSync
    . MostRecentBackup.runMostRecentBackupStateSqlite
    . ExternalDiskBackup.runExternalDiskBackup
    . Time.runCurrentTime
    . PeriodicBackup.runPeriodicBackup
    . inject
