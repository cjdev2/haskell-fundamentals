{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Maintainability.Prototypes.A_10_refactor_file_system
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
import Data.Text.IO (putStrLn)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)

class Monad m => MonadFileSystem m where
  readFile :: Text -> m Text

  default readFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> m Text
  readFile = lift . readFile

instance MonadFileSystem IO where
  readFile = T.readFile . T.unpack

main :: IO ()
main = mainLogic

mainLogic :: (MonadIO m, MonadFileSystem m) => m ()
mainLogic = do
  startTime <- liftIO getCurrentTime
  [fileName] <- liftIO getArgs
  target <- readFile $ pack fileName
  liftIO $ putStrLn $ "Hello, " <> target <> "!"
  endTime <- liftIO getCurrentTime
  let duration = endTime `diffUTCTime` startTime
      durationMicroseconds = round $ duration * 1000 * 1000 :: Integer
  liftIO $ putStrLn $ pack $ show durationMicroseconds <> " microseconds"

{-
stack runhaskell src/Maintainability/Prototypes/A_10_refactor_file_system.hs hello-target.txt
-}
