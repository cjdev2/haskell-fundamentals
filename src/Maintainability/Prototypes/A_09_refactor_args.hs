{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Maintainability.Prototypes.A_09_refactor_args
  ( main
  ) where

import Prelude hiding (putStrLn, readFile)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time.Clock as C
import qualified System.Environment as IO

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Time (MonadTime, currentTime)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (WriterT)
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.IO (putStrLn, readFile)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

class Monad m => MonadArguments m where
  getArgs :: m [Text]
  default getArgs :: (MonadTrans t, MonadArguments m', m ~ t m') => m [Text]
  getArgs = lift getArgs

instance MonadArguments IO where
  getArgs = map T.pack <$> IO.getArgs

main :: IO ()
main = mainLogic

mainLogic :: (MonadIO m, MonadArguments m) => m ()
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
stack runhaskell src/Maintainability/Prototypes/A_09_refactor_args.hs hello-target.txt
-}
