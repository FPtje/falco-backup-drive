{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Effectful.Time
-- Copyright   : © Hécate Moonlight, 2021
-- License     : MIT
-- Maintainer  : hecate@glitchbra.in
-- Stability   : stable
--
-- An effect wrapper around Data.Time for the Effectful ecosystem
-- Mostly copied from https://github.com/haskell-effectful/time-effectful Because the library
-- is not on hackage (and subsequently nixpkgs), and is too small to create a manual override for.
module Effectful.Time (
  -- * Time Effect
  Time (..),
  getCurrentTime,
  getCurrentZonedTime,

  -- * Runners
  runCurrentTime,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Kind (Type)
import Data.Time (UTCTime, ZonedTime)
import Data.Time qualified as T
import Effectful (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  type (:>),
 )
import Effectful.Dispatch.Dynamic (interpret, send)

-- | An effect for getting the current time
data Time :: Effect where
  CurrentTime :: Time m UTCTime
  CurrentZonedTime :: Time m ZonedTime

type instance DispatchOf Time = 'Dynamic

-- | Retrieve the current time in your effect stack
getCurrentTime
  :: forall (es :: [Effect])
   . Time :> es
  => Eff es UTCTime
getCurrentTime = send CurrentTime

getCurrentZonedTime
  :: forall (es :: [Effect])
   . Time :> es
  => Eff es ZonedTime
getCurrentZonedTime = send CurrentZonedTime

-- | Run time in IO
runCurrentTime
  :: forall (es :: [Effect]) (a :: Type)
   . IOE :> es
  => Eff (Time : es) a
  -> Eff es a
runCurrentTime = interpret $ \_ -> \case
  CurrentTime -> liftIO T.getCurrentTime
  CurrentZonedTime -> liftIO T.getZonedTime
