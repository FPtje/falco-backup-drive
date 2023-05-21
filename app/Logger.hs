{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Very simple effect for logging
module Logger where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Display (Display)
import Display qualified
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)

data Logger :: Effect where
  LogError :: Text -> Logger m ()
  LogInfo :: Text -> Logger m ()

makeEffect ''Logger

-- | Log everything to stdout
logStdout :: IOE :> es => Eff (Logger : es) a -> Eff es a
logStdout = interpret $ \_ -> \case
  LogError msg -> liftIO $ Text.putStrLn msg
  LogInfo msg -> liftIO $ Text.putStrLn msg

-- | Helper function to print something that has a Display instance
displayError :: (Display a, Logger :> es) => a -> Eff es ()
displayError a = logInfo $ Display.displayText a

-- | Helper function to print something that has a Display instance
displayInfo :: (Display a, Logger :> es) => a -> Eff es ()
displayInfo a = logInfo $ Display.displayText a
