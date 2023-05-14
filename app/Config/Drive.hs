{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Drive (
  MountDriveConfig (..),
  MountingMode (..),
) where

import Conferer qualified
import Conferer.FromConfig.Internal qualified as Conferer
import Conferer.Key.Internal qualified as Conferer
import Data.Text (Text)
import Display (Display (..))
import GHC.Generics (Generic)

data MountDriveConfig = MountDriveConfig
  { driveUuid :: Text
  , mountDirectory :: FilePath
  , mountingMode :: MountingMode
  , pollDelayMs :: Int
  }
  deriving (Generic)

instance Display MountDriveConfig where
  display config =
    "Mount disk uuid="
      <> display config.driveUuid
      <> " at "
      <> display config.mountDirectory
      <> " using "
      <> display config.mountingMode
      <> ", polling every "
      <> display config.pollDelayMs
      <> "ms"

data MountingMode
  = MountWithoutEncryption
  | MountWithPartitionLuks {passphrase :: !Text}

instance Display MountingMode where
  display MountWithoutEncryption = "mount without encryption"
  display MountWithPartitionLuks{} = "mount with LUKS encryption"

instance Conferer.FromConfig MountDriveConfig

instance Conferer.FromConfig MountingMode where
  fromConfig key config = do
    setting :: String <- Conferer.fetchFromConfigByIsString (key Conferer./. "type") config
    case setting of
      "without-encryption" -> pure MountWithoutEncryption
      "with-partition-luks" -> do
        passphrase <- Conferer.fetchFromConfigByIsString (key Conferer./. "passphrase") config
        pure $ MountWithPartitionLuks passphrase
      _ -> Conferer.throwMissingRequiredKeys @MountingMode [key]

instance Conferer.DefaultConfig MountDriveConfig where
  configDef =
    MountDriveConfig
      { driveUuid = ""
      , mountDirectory = "/mnt"
      , mountingMode = MountWithoutEncryption
      , pollDelayMs = 500
      }
