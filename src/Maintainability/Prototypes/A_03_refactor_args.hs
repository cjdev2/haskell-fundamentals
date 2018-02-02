{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Maintainability.A_03_refactor_args
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
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

data Arguments r where
  GetArgs :: Arguments [Text]

getArgs :: Member Arguments r => Eff r [Text]
getArgs = send GetArgs

runArgumentsIO :: (MonadIO m, LastMember m effs) => Eff (Arguments ': effs) ~> Eff effs
runArgumentsIO = interpretM $ \case
  GetArgs -> map T.pack <$> liftIO IO.getArgs

main :: IO ()
main = runM $ mainLogic & runArgumentsIO

mainLogic :: (MonadIO m, LastMember m r, Members '[Arguments] r) => Eff r ()
mainLogic = do
  startTime <- liftIO getCurrentTime
  [fileName] <- getArgs
  target <- liftIO $ readFile $ unpack fileName
  liftIO $ putStrLn $ "Hello, " <> target <> "!"
  endTime <- liftIO getCurrentTime
  let duration = endTime `diffUTCTime` startTime
      durationMicroseconds = round $ duration * 1000 * 1000 :: Integer
  liftIO $ putStrLn $ pack $ show durationMicroseconds <> " microseconds"

{-
stack runhaskell src/Maintainability/Prototypes/A_03_refactor_args.hs hello-target.txt
-}
