{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Maintainability.Prototypes.A_12_mtl
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

class Monad m => MonadArguments m where
  getArgs :: m [Text]
  default getArgs :: (MonadTrans t, MonadArguments m', m ~ t m') => m [Text]
  getArgs = lift getArgs

instance MonadArguments IO where
  getArgs = map T.pack <$> IO.getArgs

instance MonadArguments m => MonadArguments (LoggingT m)
instance MonadArguments m => MonadArguments (ReaderT r m)
instance MonadArguments m => MonadArguments (StateT s m)
instance (MonadArguments m, Monoid w) => MonadArguments (WriterT w m)

class Monad m => MonadFileSystem m where
  readFile :: Text -> m Text

  default readFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> m Text
  readFile = lift . readFile

instance MonadFileSystem IO where
  readFile = T.readFile . T.unpack

instance MonadFileSystem m => MonadFileSystem (LoggingT m)
instance MonadFileSystem m => MonadFileSystem (ReaderT r m)
instance MonadFileSystem m => MonadFileSystem (StateT s m)
instance (MonadFileSystem m, Monoid w) => MonadFileSystem (WriterT w m)

newtype AppM a = AppM (LoggingT IO a)
  deriving ( Functor, Applicative, Monad
           , MonadArguments, MonadFileSystem, MonadLogger, MonadTime )

runAppM :: AppM a -> IO a
runAppM (AppM x) = runStderrLoggingT x

main :: IO ()
main = runAppM mainLogic

mainLogic :: (MonadTime m, MonadArguments m, MonadFileSystem m, MonadLogger m) => m ()
mainLogic = do
  startTime <- currentTime
  [fileName] <- getArgs
  target <- readFile fileName
  logInfoN $ "Hello, " <> target <> "!"
  endTime <- currentTime
  let duration = endTime `diffUTCTime` startTime
      durationMicroseconds = round $ duration * 1000 * 1000 :: Integer
  logInfoN $ pack $ show durationMicroseconds <> " microseconds"

{-
stack runhaskell src/Maintainability/Prototypes/A_12_mtl.hs hello-target.txt
-}
