{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Maintainability.A_00_prepare_for_freer
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
import Data.Time.Clock (UTCTime, diffUTCTime)

data Time r where
  CurrentTime :: Time UTCTime

getCurrentTime :: Member Time r => Eff r UTCTime
getCurrentTime = send CurrentTime

runTimeIO :: (MonadIO m, LastMember m effs) => Eff (Time ': effs) ~> Eff effs
runTimeIO = interpretM $ \case
  CurrentTime -> liftIO C.getCurrentTime

data Arguments r where
  GetArgs :: Arguments [Text]

getArgs :: Member Arguments r => Eff r [Text]
getArgs = send GetArgs

runArgumentsIO :: (MonadIO m, LastMember m effs) => Eff (Arguments ': effs) ~> Eff effs
runArgumentsIO = interpretM $ \case
  GetArgs -> map T.pack <$> liftIO IO.getArgs

data FileSystem r where
  ReadFile :: Text -> FileSystem Text

readFile :: Member FileSystem r => Text -> Eff r Text
readFile = send . ReadFile

runFileSystemIO :: (MonadIO m, LastMember m effs) => Eff (FileSystem ': effs) ~> Eff effs
runFileSystemIO = interpretM $ \case
  ReadFile path -> liftIO $ T.readFile (T.unpack path)

data Log r where
  Log :: Text -> Log ()

putStrLn :: Member Log r => Text -> Eff r ()
putStrLn = send . Log

runLogIO :: (MonadIO m, LastMember m effs) => Eff (Log ': effs) ~> Eff effs
runLogIO = interpretM $ \case
  Log txt -> liftIO $ T.putStrLn txt

main :: IO ()
main = runM $ mainLogic
  & runTimeIO
  & runArgumentsIO
  & runFileSystemIO
  & runLogIO

mainLogic :: Members '[Time, Arguments, FileSystem, Log] r => Eff r ()
mainLogic = do
  startTime <- getCurrentTime
  [fileName] <- getArgs
  target <- readFile fileName
  putStrLn $ "Hello, " <> target <> "!"
  endTime <- getCurrentTime
  let duration = endTime `diffUTCTime` startTime
      durationMicroseconds = round $ duration * 1000 * 1000 :: Integer
  putStrLn $ pack $ show durationMicroseconds <> " microseconds"

{-
stack runhaskell src/Maintainability/Prototypes/A_06_freer.hs hello-target.txt
-}
