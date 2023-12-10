{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backup.RSync where

import Command (Command, CommandError)
import Command qualified
import Config.Backup.Rsync (RsyncBackupConfig (..))
import Data.Text qualified as Text
import Display (Display (..))
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem qualified as FileSystem
import Effectful.TH (makeEffect)
import Logger (Logger, displayInfo)

data RSync :: Effect where
  Run :: RsyncBackupConfig -> RSync m ()

type instance DispatchOf RSync = 'Dynamic

makeEffect ''RSync

runRSync
  :: (Error.HasCallStack, IOE :> es, Command :> es, Logger :> es, Error CommandError :> es, Logger :> es)
  => Eff (RSync : es) a
  -> Eff es a
runRSync = reinterpret FileSystem.runFileSystem $ \_ -> \case
  Run config -> do
    FileSystem.createDirectoryIfMissing True config.backupDestination

    Logger.displayInfo $ "Starting rsync backup " <> display config.backupName
    Command.runProcessThrowOnError
      "rsync"
      ( [ "--archive"
        , "--fsync"
        , config.backupSource
        , config.backupDestination
        ]
          <> map Text.unpack config.extraArgs
      )
      ""
    Logger.displayInfo $ "Finished rsync backup " <> display config.backupName
