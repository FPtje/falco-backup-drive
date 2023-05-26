{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Backup.PeriodicBackup where

import Config.State (BackupName)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Display (Display (..))
import GHC.Generics (Generic)

data PeriodicBackupConfig subconfig = PeriodicBackupConfig
  { scheduleIdentifier :: BackupName
  , backupInterval :: NominalDiffTime
  , backup :: subconfig
  }
  deriving (Generic, FromJSON, ToJSON)

instance Display subconfig => Display (PeriodicBackupConfig subconfig) where
  display config =
    "Backing up every "
      <> display (formatBackupInterval config.backupInterval)
      <> " under id="
      <> display config.scheduleIdentifier
      <> ":\n  "
      <> display config.backup

-- | Helper to format a difftime for backup use
formatBackupInterval :: NominalDiffTime -> String
formatBackupInterval interval =
  formatTime
    defaultTimeLocale
    "%H hours, %M minutes, %S seconds"
    $ posixSecondsToUTCTime
    $ realToFrac interval
