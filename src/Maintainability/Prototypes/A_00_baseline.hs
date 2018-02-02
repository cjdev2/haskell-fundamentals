{-# LANGUAGE OverloadedStrings #-}

module Maintainability.Prototypes.A_00_baseline
  ( main
  ) where

import Prelude hiding (putStrLn, readFile)
import Data.Semigroup ((<>))
import Data.Text (pack)
import Data.Text.IO (putStrLn, readFile)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)

main :: IO ()
main = do
  startTime <- getCurrentTime
  [fileName] <- getArgs
  target <- readFile fileName
  putStrLn $ "Hello, " <> target <> "!"
  endTime <- getCurrentTime
  let duration = endTime `diffUTCTime` startTime
      durationMicroseconds = round $ duration * 1000 * 1000 :: Integer
  putStrLn $ pack $ show durationMicroseconds <> " microseconds"

{-
stack runhaskell src/Maintainability/Prototypes/A_00_baseline.hs hello-target.txt
-}
