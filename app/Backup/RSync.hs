{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backup.RSync where

import Command (Command, CommandError)
import Command qualified
import Config.Backup.Rsync (RsyncBackupConfig (..))
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Error.Static (Error)
import Effectful.FileSystem qualified as FileSystem
import Effectful.TH (makeEffect)

data RSync :: Effect where
  Run :: RsyncBackupConfig -> RSync m ()

type instance DispatchOf RSync = 'Dynamic

makeEffect ''RSync

runRSync
  :: (IOE :> es, Command :> es, Error CommandError :> es)
  => Eff (RSync : es) a
  -> Eff es a
runRSync = reinterpret FileSystem.runFileSystem $ \_ -> \case
  Run config -> do
    FileSystem.createDirectoryIfMissing True config.backupDestination

    Command.runProcessThrowOnError
      "rsync"
      [ "--archive"
      , "--fsync"
      , config.backupSource
      , config.backupDestination
      ]
      ""
