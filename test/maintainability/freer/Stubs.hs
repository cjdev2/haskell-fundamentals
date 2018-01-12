{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Maintainability.Freer.Stubs where

import qualified Data.Text as T

import Control.Monad.Freer (Eff, interpret, reinterpret)
import Control.Monad.Freer.State (State, evalState, get, put)
import Control.Monad.Freer.Writer (runWriter, tell)
import Control.Natural (type (~>))
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)

import Maintainability.Freer.Effects

--------------------------------------------------------------------------------
-- Simple effects

runArgumentsPure :: [Text] -> Eff (Arguments ': effs) ~> Eff effs
runArgumentsPure args = interpret $ \case
  GetArgs -> pure args

runFileSystemPure :: [(Text, Text)] -> Eff (FileSystem ': effs) ~> Eff effs
runFileSystemPure fs = interpret $ \case
  ReadFile path ->
    maybe (fail $ "readFile: no such file ‘" ++ T.unpack path ++ "’")
          pure (lookup path fs)

runLogPure :: Eff (Log ': effs) a -> Eff effs (a, [Text])
runLogPure = runWriter . reinterpret (\case
    Log txt -> tell [txt])

--------------------------------------------------------------------------------
-- Time

data ClockState
  = ClockStopped !UTCTime
  | ClockTick !UTCTime ClockState
  | ClockEndOfTime
  deriving (Eq, Show)

runClockPure :: ClockState -> Eff (Time ': effs) ~> Eff effs
runClockPure initialState action = evalState initialState (handle action)
  where
    handle :: Eff (Time ': effs) ~> Eff (State ClockState ': effs)
    handle = reinterpret $ \case
      CurrentTime -> get >>= \case
        ClockStopped t -> pure t
        ClockTick t s -> put s >> pure t
        ClockEndOfTime -> fail "currentTime: end of time"

-- | Runs a computation with a constant time that never changes.
runStoppedClockPure :: UTCTime -> Eff (Time ': effs) ~> Eff effs
runStoppedClockPure time = runClockPure (ClockStopped time)

-- | Runs a computation with a clock that advances by 1 second every time the
-- time is read.
runTickingClockPure :: UTCTime -> Eff (Time ': effs) ~> Eff effs
runTickingClockPure = runTickingClockPure' 1

-- | Runs a computation with a clock that advances by the given interval every
-- time the time is read.
runTickingClockPure' :: NominalDiffTime -> UTCTime -> Eff (Time ': effs) ~> Eff effs
runTickingClockPure' d t = runClockPure (ticks t)
  where ticks t' = ClockTick t' (ticks (addUTCTime d t'))

-- | Runs a computation with a clock that replays the provided list of times, in
-- order. If the time list of times is exhausted, 'currentTime' will throw an
-- exception the next time it is called.
runPresetClockPure :: [UTCTime] -> Eff (Time ': effs) ~> Eff effs
runPresetClockPure ts = runClockPure (foldr ClockTick ClockEndOfTime ts)
