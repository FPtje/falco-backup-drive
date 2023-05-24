{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The top level config that combines the other configs
module Config.TopLevel where

import Config.Backup.ExternalDisk (ExternalDiskBackupConfig)
import Config.Backup.Rsync (RsyncBackupConfig)
import Config.Drive (MountDriveConfig)
import Config.State (StateConfig (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.List (foldl')
import Display (Display (..))
import GHC.Generics (Generic)

data Config = Config
  { state :: StateConfig
  , mountBackupDrive :: Maybe MountDriveConfig
  , rsyncBackups :: [RsyncBackupConfig]
  , externalDiskBackups :: [ExternalDiskBackupConfig]
  }
  deriving (Generic, ToJSON, FromJSON)

instance Display Config where
  display config =
    "Backup config:\n  "
      <> maybe "Not mounting any drives" display config.mountBackupDrive
      <> "\nRsync backups:"
      <> foldl' (\acc c -> acc <> "\n  " <> display c) "" config.rsyncBackups
      <> "\nExternal disk backups:"
      <> foldl' (\acc c -> acc <> "\n  " <> display c) "" config.externalDiskBackups

defaultConfig :: Config
defaultConfig =
  Config
    { state = StateConfig ":memory:"
    , mountBackupDrive = Nothing
    , rsyncBackups = []
    , externalDiskBackups = []
    }
