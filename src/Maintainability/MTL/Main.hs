-- stack runhaskell -- -isrc src/Maintainability/MTL/Main.hs hello-target.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Maintainability.MTL.Main
  ( main
  , mainLogic
  ) where

import qualified Data.Text as T

import Prelude hiding (readFile)

import Control.Monad.Time (MonadTime(..))
import Control.Monad.Logger (LoggingT, MonadLogger(..), logInfoN, runStderrLoggingT)
import Data.Semigroup ((<>))
import Data.Time.Clock (diffUTCTime)

import Maintainability.MTL.Interfaces

--------------------------------------------------------------------------------
-- IO wiring

newtype AppM a = AppM (LoggingT IO a)
  deriving ( Functor, Applicative, Monad
           , MonadArguments, MonadFileSystem, MonadLogger, MonadTime )

runAppM :: AppM a -> IO a
runAppM (AppM x) = runStderrLoggingT x

main :: IO ()
main = runAppM mainLogic

--------------------------------------------------------------------------------
-- Logic

mainLogic :: (MonadArguments m, MonadFileSystem m, MonadLogger m, MonadTime m) => m ()
mainLogic = do
  startTime <- currentTime
  [fileName] <- getArgs
  target <- readFile fileName
  logInfoN $ "Hello, " <> target <> "!"
  endTime <- currentTime
  let duration = endTime `diffUTCTime` startTime
  logInfoN $ T.pack (show (round (duration * 1000 * 1000) :: Integer)) <> " microseconds"
