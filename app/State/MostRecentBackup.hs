{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Stores state for the most recent backup times
module State.MostRecentBackup (
  MostRecentBackup (..),
  BackupName (..),
  MostRecentBackupState,
  runMostRecentBackupStateSqlite,
  storeBackupTime,
  getMostRecentBackup,
  migrateTables,
) where

import Config.State (BackupName (..), StateConfig (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental qualified as E
import Database.Persist qualified as Persist
import Database.Persist.Sqlite qualified as Persist
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Persistent.SqliteEffect (Sqlite)
import Effectful.Persistent.SqliteEffect qualified as Sqlite
import Effectful.TH (makeEffect)

share
  [mkPersist sqlSettings, mkMigrate "migrateMostRecentBackup"]
  [persistLowerCase|
  MostRecentBackup
    name Text
    created UTCTime default=CURRENT_TIME
    UniqueName name
 |]

-- | Stores the most recent backup state
data MostRecentBackupState :: Effect where
  StoreBackupTime :: StateConfig -> BackupName -> UTCTime -> MostRecentBackupState m ()
  GetMostRecentBackup :: StateConfig -> BackupName -> MostRecentBackupState m (Maybe UTCTime)

makeEffect ''MostRecentBackupState

runMostRecentBackupStateSqlite
  :: (IOE :> es, Sqlite :> es)
  => Eff (MostRecentBackupState : es) a
  -> Eff es a
runMostRecentBackupStateSqlite = interpret $ \_ -> \case
  StoreBackupTime stateConfig backupName time ->
    Sqlite.runSqlite stateConfig.sqliteFilePath $ do
      E.delete $ deleteBackupsByName backupName.name
      _insertedKey <- Persist.insert $ MostRecentBackup backupName.name time
      pure ()
  GetMostRecentBackup stateConfig backupName -> do
    mostRecentTime <-
      Sqlite.runSqlite stateConfig.sqliteFilePath $
        E.selectOne $
          mostRecentBackupQuery backupName.name
    pure $ fmap E.unValue mostRecentTime

-- | Run the sqlite table migration
migrateTables :: (IOE :> es, Sqlite :> es) => StateConfig -> Eff es ()
migrateTables stateConfig =
  Sqlite.runSqlite stateConfig.sqliteFilePath $
    Persist.runMigration migrateMostRecentBackup

mostRecentBackupQuery :: Text -> E.SqlQuery (E.SqlExpr (E.Value UTCTime))
mostRecentBackupQuery backupName = do
  mostRecentBackup <- E.from (E.table @MostRecentBackup)
  E.where_ (mostRecentBackup E.^. MostRecentBackupName E.==. E.val backupName)
  pure $ mostRecentBackup E.^. MostRecentBackupCreated

deleteBackupsByName :: Text -> E.SqlQuery ()
deleteBackupsByName backupName = do
  mostRecentBackup <- E.from (E.table @MostRecentBackup)
  E.where_ (mostRecentBackup E.^. MostRecentBackupName E.==. E.val backupName)
  pure ()
