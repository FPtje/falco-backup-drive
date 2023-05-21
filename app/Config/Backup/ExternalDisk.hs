{-# LANGUAGE DeriveAnyClass #-}
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
  }
  deriving (Generic, FromJSON, ToJSON)

instance Display ExternalDiskBackupConfig where
  display config =
    display config.mountConfig <> ", and then backup using " <> display config.rsyncConfig
