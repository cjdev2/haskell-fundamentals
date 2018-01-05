module MaintainableCode.Main
  ( main
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude hiding (log, readFile)

import Data.Semigroup ((<>))
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)

main :: IO ()
main = do
  startTime <- getCurrentTime
  [fileName] <- getArgs
  target <- T.readFile fileName
  T.putStrLn $ "Hello, " <> target <> "!"
  endTime <- getCurrentTime
  let duration = endTime `diffUTCTime` startTime
  T.putStrLn $ T.pack (show (round (duration * 1000) :: Integer)) <> " milliseconds"
