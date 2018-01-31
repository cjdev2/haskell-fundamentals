-- stack runhaskell -- -isrc src/Maintainability/Freer/Main.hs hello-target.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Maintainability.Freer.Main
  ( main
  , mainLogic
  ) where

import qualified Data.Text as T

import Prelude hiding (log, readFile)

import Control.Monad.Freer (Eff, Members, runM)
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Time.Clock (diffUTCTime)

import Maintainability.Freer.Effects

--------------------------------------------------------------------------------
-- IO wiring

main :: IO ()
main = runM $ mainLogic
  & runArgumentsIO
  & runFileSystemIO
  & runLogIO
  & runTimeIO

--------------------------------------------------------------------------------
-- Logic

mainLogic :: Members '[Arguments, FileSystem, Log, Time] r => Eff r ()
mainLogic = do
  startTime <- currentTime
  [fileName] <- getArgs
  target <- readFile fileName
  log $ "Hello, " <> target <> "!"
  endTime <- currentTime
  let duration = endTime `diffUTCTime` startTime
  log $ T.pack (show (round (duration * 1000) :: Integer)) <> " milliseconds"
