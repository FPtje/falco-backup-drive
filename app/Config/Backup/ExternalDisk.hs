{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Backup.ExternalDisk where

import Config.Backup.Rsync (RsyncBackupConfig)
import Config.Drive (MountDriveConfig)
import Data.Aeson (FromJSON, ToJSON)
import Display (Display (..))
import GHC.Generics (Generic)

data ExternalDiskBackupConfig = ExternalDiskBackupConfig
  { mountConfig :: MountDriveConfig
  , rsyncConfig :: RsyncBackupConfig
  , formatAfterBackup :: FormatOption
  }
  deriving (Generic, FromJSON, ToJSON)

data FormatOption
  = DoNotFormat
  | FormatExfat
  deriving (Generic, FromJSON, ToJSON)

instance Display ExternalDiskBackupConfig where
  display config =
    display config.mountConfig
      <> ", and then backup using "
      <> display config.rsyncConfig
      <> ". "
      <> display config.formatAfterBackup

instance Display FormatOption where
  display = \case
    DoNotFormat -> "There will be NO formatting after the backup"
    FormatExfat -> "The drive will be formatted with exfat after the backup"
