{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Drive (
  MountDriveConfig (..),
  MountingMode (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Display (Display (..))
import GHC.Generics (Generic)

data MountDriveConfig = MountDriveConfig
  { drivePath :: FilePath
  , driveName :: Text
  , mountDirectory :: FilePath
  , mountingMode :: MountingMode
  , pollDelayMs :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

instance Display MountDriveConfig where
  display config =
    "Mount disk path="
      <> display config.drivePath
      <> " at "
      <> display config.mountDirectory
      <> " using "
      <> display config.mountingMode
      <> ", polling every "
      <> display config.pollDelayMs
      <> "ms"

data MountingMode
  = MountWithoutEncryption
  | MountWithPartitionLuks
  deriving (Generic, FromJSON, ToJSON)

instance Display MountingMode where
  display MountWithoutEncryption = "mount without encryption"
  display MountWithPartitionLuks{} = "mount with LUKS encryption"
