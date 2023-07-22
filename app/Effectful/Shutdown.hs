{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.Shutdown (OnShutdown, onShutdown, runOnShutdown) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async qualified as Concurrent
import Effectful.Dispatch.Dynamic (LocalEnv, localSeqLift, localSeqUnliftIO, reinterpret)
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Effectful.TH (makeEffect)
import System.Exit (exitSuccess)
import System.Posix.Signals qualified as Signals

data OnShutdown :: Effect where
  OnShutdown :: IO () -> OnShutdown m ()

makeEffect ''OnShutdown

runOnShutdown
  :: forall es a
   . (Concurrent :> es, IOE :> es)
  => Eff (OnShutdown : es) a
  -> Eff es a
runOnShutdown = reinterpret (State.evalState @[IO ()] []) $ \env -> \case
  OnShutdown m -> do
    firstShutdownToBeRegistered <- State.state @[IO ()] $ \s ->
      (null s, m : s)

    -- When this function is called for the first time, register the actual handlers
    when firstShutdownToBeRegistered $ registerHandlers env

-- | Installs the signal handlers. This should only be called once for the duration of the program.
registerHandlers
  :: (Concurrent :> es, IOE :> es, State [IO ()] :> es)
  => LocalEnv localEs es
  -> Eff es ()
registerHandlers env = do
  localSeqLift env $ \unliftEff -> localSeqUnliftIO env $ \unliftIO -> do
    let
      onSignal :: IO ()
      onSignal = unliftIO $ unliftEff $ do
        ms <- State.get @[IO ()]
        Concurrent.mapConcurrently_ liftIO ms
        liftIO exitSuccess

    for_ [Signals.sigTERM, Signals.sigINT] $ \sig ->
      Signals.installHandler sig (Signals.CatchOnce onSignal) Nothing
