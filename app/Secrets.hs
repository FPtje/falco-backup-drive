{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Very simple module for secrets management
module Secrets (
  SecretPath (..),
  Secret (..),
  SecretError (..),
  Secrets,
  getSecret,
  runSecrets,
) where

import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Display (Display (..))
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Environment
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.TH (makeEffect)

newtype SecretPath = SecretPath {path :: String}
  deriving (IsString)

newtype Secret = Secret {value :: Text}

instance Display Secret where
  display _ = "<secret>"

data SecretError
  = SecretDoesNotExist SecretPath

instance Display SecretError where
  display = \case
    SecretDoesNotExist secretPath -> "Secret does not exist: " <> display secretPath.path

data Secrets :: Effect where
  GetSecret :: SecretPath -> Secrets m Secret

makeEffect ''Secrets

-- | Simple interpreter that just reads from an environment variable
runSecrets :: (Error SecretError :> es, Environment :> es) => Eff (Secrets : es) a -> Eff es a
runSecrets = interpret $ \_ -> \case
  GetSecret secretPath -> do
    mbSecret <- Environment.lookupEnv secretPath.path
    case mbSecret of
      Nothing -> Error.throwError $ SecretDoesNotExist secretPath
      Just secret -> pure $ Secret $ Text.pack secret
