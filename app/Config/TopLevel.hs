{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The top level config that combines the other configs
module Config.TopLevel where

import Config.Drive (MountDriveConfig)
import Config.RsyncBackup (RsyncBackupConfig)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (foldl')
import Display (Display (..))
import GHC.Generics (Generic)

data Config = Config
  { mountBackupDrive :: Maybe MountDriveConfig
  , rsyncBackups :: [RsyncBackupConfig]
  }
  deriving (Generic, ToJSON, FromJSON)

instance Display Config where
  display config =
    "Backup config:\n  "
      <> maybe "Not mounting any drives" display config.mountBackupDrive
      <> "\nRsync backups:\n"
      <> foldl' (\acc c -> acc <> "\n" <> display c) "" config.rsyncBackups

defaultConfig :: Config
defaultConfig =
  Config
    { mountBackupDrive = Nothing
    , rsyncBackups = []
    }
