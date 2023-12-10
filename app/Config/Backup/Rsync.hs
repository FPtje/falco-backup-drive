{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Backup.Rsync where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Display (Display (..))
import GHC.Generics (Generic)

data RsyncBackupConfig = RsyncBackupConfig
  { backupName :: Text
  , backupSource :: FilePath
  , backupDestination :: FilePath
  , extraArgs :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON)

instance Display RsyncBackupConfig where
  display config =
    "RSync \""
      <> display config.backupName
      <> "\" with args '"
      <> display (Text.unwords $ map (\t -> "\"" <> t <> "\"") config.extraArgs)
      <> "' from "
      <> display config.backupSource
      <> " to "
      <> display config.backupDestination
