module MaintainableCode.Main
  ( main
  , mainIO
  ) where

import qualified Data.Text as T

import Prelude hiding (log, readFile)

import Control.Monad.Freer (Eff, Members, runM)
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Time.Clock (diffUTCTime)

import MaintainableCode.Effects

--------------------------------------------------------------------------------
-- IO wiring

mainIO :: IO ()
mainIO = runM $ main
  & runArgumentsIO
  & runFileSystemIO
  & runLogIO
  & runTimeIO

--------------------------------------------------------------------------------
-- Logic

main :: Members '[Arguments, FileSystem, Log, Time] r => Eff r ()
main = do
  startTime <- currentTime
  [fileName] <- getArgs
  target <- readFile fileName
  log $ "Hello, " <> target <> "!"
  endTime <- currentTime
  let duration = endTime `diffUTCTime` startTime
  log $ T.pack (show (round (duration * 1000) :: Integer)) <> " milliseconds"