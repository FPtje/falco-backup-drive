{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Extends the effectful error module with some helper functions
module Effectful.Error where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Builder qualified as BS
import Display (Display (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Logger (Logger)
import Logger qualified
import System.Exit (exitFailure)

-- | Run an effect, and on failure, print the error and exit with failure
runFailOnError
  :: forall e es a
   . (IOE :> es, Logger :> es, Display e)
  => Eff (Error e : es) a
  -> Eff es a
runFailOnError eff = do
  eError <- Error.runError @e eff
  case eError of
    Left err -> do
      Logger.displayError $ displayError err
      liftIO exitFailure
    Right res -> pure res

-- | Simple function to pretty print an error
displayError :: Display e => (Error.CallStack, e) -> BS.Builder
displayError (callstack, err) =
  display (Error.prettyCallStack callstack) <> "\n" <> display err
