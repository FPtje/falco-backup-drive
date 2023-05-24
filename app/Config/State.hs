{-# LANGUAGE DeriveAnyClass #-}

module Config.State where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data StateConfig = StateConfig
  { sqliteFilePath :: Text
  }
  deriving (Generic, ToJSON, FromJSON)
