{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Maintainability.Freer.Effects
  ( -- * Effects
    Arguments(..)
  , getArgs
  , FileSystem(..)
  , readFile
  , Log(..)
  , log
  , Time(..)
  , currentTime

    -- * Effect handlers
  , runArgumentsIO
  , runFileSystemIO
  , runLogIO
  , runTimeIO
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Environment as IO

import Prelude hiding (log, readFile)

import Control.Monad.Freer (Eff, LastMember, Member, interpretM, send)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Natural (type (~>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)

--------------------------------------------------------------------------------
-- Interfaces

data Arguments r where
  GetArgs :: Arguments [Text]

getArgs :: Member Arguments r => Eff r [Text]
getArgs = send GetArgs


data FileSystem r where
  ReadFile :: Text -> FileSystem Text

readFile :: Member FileSystem r => Text -> Eff r Text
readFile = send . ReadFile


data Log r where
  Log :: Text -> Log ()

log :: Member Log r => Text -> Eff r ()
log = send . Log


data Time r where
  CurrentTime :: Time UTCTime

currentTime :: Member Time r => Eff r UTCTime
currentTime = send CurrentTime

--------------------------------------------------------------------------------
-- IO implementations

runArgumentsIO :: (MonadIO m, LastMember m effs) => Eff (Arguments ': effs) ~> Eff effs
runArgumentsIO = interpretM $ \case
  GetArgs -> map T.pack <$> liftIO IO.getArgs

runFileSystemIO :: (MonadIO m, LastMember m effs) => Eff (FileSystem ': effs) ~> Eff effs
runFileSystemIO = interpretM $ \case
  ReadFile path -> liftIO $ T.readFile (T.unpack path)

runLogIO :: (MonadIO m, LastMember m effs) => Eff (Log ': effs) ~> Eff effs
runLogIO = interpretM $ \case
  Log txt -> liftIO $ T.putStrLn txt

runTimeIO :: (MonadIO m, LastMember m effs) => Eff (Time ': effs) ~> Eff effs
runTimeIO = interpretM $ \case
  CurrentTime -> liftIO getCurrentTime
