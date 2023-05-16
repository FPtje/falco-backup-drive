{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backup.RSync where

import Command (Command, CommandError)
import Command qualified
import Config.RsyncBackup (RsyncBackupConfig (..))
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Error.Static (Error)
import Effectful.FileSystem qualified as FileSystem
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static as Reader (ask)
import Effectful.TH (makeEffect)

data RSync :: Effect where
  Run :: RSync m ()

makeEffect ''RSync

runRSync
  :: (IOE :> es, Reader RsyncBackupConfig :> es, Command :> es, Error CommandError :> es)
  => Eff (RSync : es) a
  -> Eff es a
runRSync = reinterpret FileSystem.runFileSystem $ \_ -> \case
  Run -> do
    config <- Reader.ask @RsyncBackupConfig
    FileSystem.createDirectoryIfMissing True config.backupDestination

    Command.runProcessThrowOnError
      "rsync"
      [ "--archive"
      , config.backupSource
      , config.backupDestination
      ]
      ""
