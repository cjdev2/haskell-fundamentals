-- stack runhaskell src/Maintainability/Unmaintainable/Main.hs hello-target.txt

{-# LANGUAGE OverloadedStrings #-}

module Maintainability.Unmaintainable.Main
  ( main
  ) where

import Prelude hiding (readFile, putStrLn)
import Data.Text.IO (readFile, putStrLn)
import Data.Text (pack)
import Data.Semigroup ((<>))
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
