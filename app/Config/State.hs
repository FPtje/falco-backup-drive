{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Config.State where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Display (Display)
import GHC.Generics (Generic)

newtype BackupName = BackupName {name :: Text}
  deriving newtype (FromJSON, ToJSON, Display)

data StateConfig = StateConfig
  { sqliteFilePath :: Text
  }
  deriving (Generic, ToJSON, FromJSON)
