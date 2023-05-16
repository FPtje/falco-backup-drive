{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Config.GetConfig where

import Config.TopLevel (Config)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)

data GetConfig :: Effect where
  ReadConfig :: GetConfig m Config
  WriteConfig :: Config -> GetConfig m ()

makeEffect ''GetConfig

runGetConfig :: IOE :> es => Eff (GetConfig : es) a -> Eff es a
runGetConfig = interpret $ \_ -> \case
  ReadConfig -> do
    fileContents <- liftIO $ BS.readFile "config.json"
    liftIO $ Aeson.throwDecodeStrict' fileContents
  WriteConfig config ->
    liftIO $ Aeson.encodeFile "config.json" config
