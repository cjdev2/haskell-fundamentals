{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Maintainability.A_05_refactor_console_output
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
import Data.Text.IO (readFile)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Environment (getArgs)

data Log r where
  Log :: Text -> Log ()

putStrLn :: Member Log r => Text -> Eff r ()
putStrLn = send . Log

runLogIO :: (MonadIO m, LastMember m effs) => Eff (Log ': effs) ~> Eff effs
runLogIO = interpretM $ \case
  Log txt -> liftIO $ T.putStrLn txt

main :: IO ()
main = runM $ mainLogic & runLogIO

mainLogic :: (MonadIO m, LastMember m r, Members '[Log] r) => Eff r ()
mainLogic = do
  startTime <- liftIO getCurrentTime
  [fileName] <- liftIO getArgs
  target <- liftIO $ readFile fileName
  putStrLn $ "Hello, " <> target <> "!"
  endTime <- liftIO getCurrentTime
  let duration = endTime `diffUTCTime` startTime
      durationMicroseconds = round $ duration * 1000 * 1000 :: Integer
  putStrLn $ pack $ show durationMicroseconds <> " microseconds"

{-
stack runhaskell src/Maintainability/Prototypes/A_05_refactor_console_output.hs hello-target.txt
-}
