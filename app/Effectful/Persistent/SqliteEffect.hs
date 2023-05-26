{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.Persistent.SqliteEffect where

import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Logger.CallStack (mapNoLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as ReaderT
import Control.Monad.Trans.Resource (ResourceT, transResourceT)
import Data.Text (Text)
import Database.Persist.Sqlite qualified as Persist
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO)
import Effectful.TH (makeEffect)

-- | An effect that represents a subset of the functions in persistent-sqlite. Persistent is coupled
-- to mtl-style transformers from mtl, monad-logger, and resourcet. This effect is a middle ground
-- to using these mtl-style effects, but also still staying within the Eff monad.
data Sqlite :: Effect where
  RunSqlite :: Text -> ReaderT Persist.SqlBackend (NoLoggingT (ResourceT m)) a -> Sqlite m a

makeEffect ''Sqlite

runSqliteIO :: IOE :> es => Eff (Sqlite : es) a -> Eff es a
runSqliteIO = interpret $ \env -> \case
  RunSqlite connectionString m ->
    localSeqUnliftIO env $ \unlift ->
      Persist.runSqlite
        connectionString
        (ReaderT.mapReaderT (mapNoLoggingT (transResourceT unlift)) m)
