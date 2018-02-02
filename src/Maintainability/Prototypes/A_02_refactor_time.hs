{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Maintainability.A_02_refactor_time
  ( main
  ) where

import Prelude hiding (putStrLn, readFile)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time.Clock as C
import qualified System.Environment as IO

import Control.Monad.Freer (Eff, LastMember, Member, Members, interpretM, runM, send)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Natural (type (~>))
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.IO (putStrLn, readFile)
import Data.Time.Clock (UTCTime, diffUTCTime)
import System.Environment (getArgs)

data Time r where
  CurrentTime :: Time UTCTime

getCurrentTime :: Member Time r => Eff r UTCTime
getCurrentTime = send CurrentTime

runTimeIO :: (MonadIO m, LastMember m effs) => Eff (Time ': effs) ~> Eff effs
runTimeIO = interpretM $ \case
  CurrentTime -> liftIO C.getCurrentTime

main :: IO ()
main = runM $ mainLogic & runTimeIO

mainLogic :: (MonadIO m, LastMember m r, Members '[Time] r) => Eff r ()
mainLogic = do
  startTime <- getCurrentTime
  [fileName] <- liftIO getArgs
  target <- liftIO $ readFile fileName
  liftIO $ putStrLn $ "Hello, " <> target <> "!"
  endTime <- getCurrentTime
  let duration = endTime `diffUTCTime` startTime
      durationMicroseconds = round $ duration * 1000 * 1000 :: Integer
  liftIO $ putStrLn $ pack $ show durationMicroseconds <> " microseconds"

{-
stack runhaskell src/Maintainability/Prototypes/A_02_refactor_time.hs hello-target.txt
-}
