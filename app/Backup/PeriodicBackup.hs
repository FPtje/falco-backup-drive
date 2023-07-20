{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Loop a backup, and make sure it runs only once every X seconds/minutes/hours/days.
module Backup.PeriodicBackup where

import Config.Backup.PeriodicBackup (PeriodicBackupConfig (..), formatBackupInterval)
import Config.State (StateConfig)
import Data.Time (NominalDiffTime, addUTCTime, diffUTCTime)
import Display (Display (..))
import Effectful (Eff, Effect, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as Concurrent
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static qualified as Error
import Effectful.TH (makeEffect)
import Effectful.Time (Time, getCurrentTime)
import Effectful.Time qualified as Time
import Logger (Logger)
import Logger qualified
import State.MostRecentBackup (MostRecentBackupState, getMostRecentBackup, storeBackupTime)

data TimeToNextBackup
  = RunAsap
  | RunIn NominalDiffTime

data PeriodicBackup :: Effect where
  GetNextTimeToBackup :: StateConfig -> PeriodicBackupConfig config -> PeriodicBackup m TimeToNextBackup

makeEffect ''PeriodicBackup

-- | Run periodic backup effect
runPeriodicBackup
  :: (Error.HasCallStack, MostRecentBackupState :> es, Time :> es)
  => Eff (PeriodicBackup : es) a
  -> Eff es a
runPeriodicBackup = interpret $ \_ -> \case
  GetNextTimeToBackup stateConfig config -> do
    mbMostRecentBackup <- getMostRecentBackup stateConfig config.scheduleIdentifier
    time <- getCurrentTime
    case mbMostRecentBackup of
      Nothing -> pure RunAsap
      Just mostRecentBackup ->
        if mostRecentBackup <= addUTCTime (-config.backupInterval) time
          then pure RunAsap
          else pure $ RunIn $ diffUTCTime (addUTCTime config.backupInterval mostRecentBackup) time

loopPeriodicBackup
  :: ( Error.HasCallStack
     , MostRecentBackupState :> es
     , Concurrent :> es
     , PeriodicBackup :> es
     , Time :> es
     , Logger :> es
     )
  => StateConfig
  -> PeriodicBackupConfig config
  -> Eff es ()
  -> Eff es ()
loopPeriodicBackup stateConfig config runBackup = go
 where
  go = do
    nextBackup <- getNextTimeToBackup stateConfig config
    case nextBackup of
      RunAsap ->
        Logger.displayInfo $
          "Running backup " <> display config.scheduleIdentifier <> " immediately"
      RunIn delay -> do
        Logger.displayInfo $
          "Running backup "
            <> display config.scheduleIdentifier
            <> " after a delay of "
            <> display (formatBackupInterval delay)
        Concurrent.threadDelay $ truncate $ delay * 1000000

    timeBeforeBackup <- Time.getCurrentTime
    runBackup
    storeBackupTime stateConfig config.scheduleIdentifier timeBeforeBackup
    go
