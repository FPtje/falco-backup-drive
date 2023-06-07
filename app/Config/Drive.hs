{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Drive (
  MountDriveConfig (..),
  MountingMode (..),
  FsckMode (..)
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
  , fsck :: FsckMode
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
      <> "ms, "
      <> display config.fsck

data MountingMode
  = MountWithoutEncryption
  | MountWithPartitionLuks
  deriving (Generic, FromJSON, ToJSON)

instance Display MountingMode where
  display MountWithoutEncryption = "mount without encryption"
  display MountWithPartitionLuks{} = "mount with LUKS encryption"

data FsckMode
  = DoNotFsck
  | FsckRepairExfat
  deriving (Generic, FromJSON, ToJSON)

instance Display FsckMode where
  display DoNotFsck = "not running fsck"
  display FsckRepairExfat = "running `fsck.exfat --repair-yes` before mount"
