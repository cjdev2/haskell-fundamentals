{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Maintainability.A_04_refactor_file_system
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
import Data.Text.IO (putStrLn)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Environment (getArgs)

data FileSystem r where
  ReadFile :: Text -> FileSystem Text

readFile :: Member FileSystem r => Text -> Eff r Text
readFile = send . ReadFile

runFileSystemIO :: (MonadIO m, LastMember m effs) => Eff (FileSystem ': effs) ~> Eff effs
runFileSystemIO = interpretM $ \case
  ReadFile path -> liftIO $ T.readFile (T.unpack path)

main :: IO ()
main = runM $ mainLogic & runFileSystemIO

mainLogic :: (MonadIO m, LastMember m r, Members '[FileSystem] r) => Eff r ()
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
stack runhaskell src/Maintainability/Prototypes/A_04_refactor_file_system.hs hello-target.txt
-}
