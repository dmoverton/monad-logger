{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Logger.Mock
    ( MockLoggerT
    , runMockLoggerT
    , execMockLoggerT
    , evalMockLoggerT
    , PureMockLogger
    , runPureMockLogger
    , execPureMockLogger
    , evalPureMockLogger
    , logShouldBe
    , logsShouldBe
    , logPropsContain
    , LogMessage(..)
    ) where

import           Control.Monad (zipWithM_)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.Except (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State (State, evalState, execState, modify', runState)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Aeson
import           Data.HashMap.Strict (intersection)
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import           Data.Text (Text)
import           Test.Hspec

import           Control.Monad.Logger (LogLevel(..), LogMessage(..), MonadLogger(..))

-- | MockLoggerT maintains its state in an IORef so that logs are not lost when an exception
-- is thrown. If using StateT instead to record logs, only the logs that were recorded at the
-- point of the 'catch' will be returned; other logs will be discarded.
-- This behaviour properly models how actual logging is done (ie logs done before throwing
-- an exception are printed to stdout and are not lost)
newtype MockLoggerT m a = MockLoggerT (ReaderT (IORef [LogMessage]) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadError e, MonadBase b)

instance MonadIO m => MonadLogger (MockLoggerT m) where
  log logMessage = MockLoggerT $ modify (++ [logMessage])
  ambientLogProperties _ a = a -- unneeded as yet, so havent provided implementation

instance PrimMonad m => PrimMonad (MockLoggerT m) where
  type PrimState (MockLoggerT m) = PrimState m
  primitive f = MockLoggerT $ primitive f

modify :: MonadIO m => (a -> a) -> ReaderT (IORef a) m ()
modify fn = ask >>= \ref -> liftIO $ modifyIORef' ref fn

runMockLoggerT :: MonadIO m => MockLoggerT m a -> m (a, [LogMessage])
runMockLoggerT (MockLoggerT s) = do
  logRef <- liftIO $ newIORef []
  result <- runReaderT s logRef
  liftIO $ (result,) <$> readIORef logRef

execMockLoggerT :: MonadIO m => MockLoggerT m a -> m [LogMessage]
execMockLoggerT = fmap snd . runMockLoggerT

evalMockLoggerT :: MonadIO m => MockLoggerT m a -> m a
evalMockLoggerT = fmap fst . runMockLoggerT

-- | PureMockLogger is a pure logging monad for use at the base of a transformer stack
-- in cases where you must have pure code (ie. no IO stuff like MockLoggerT)
newtype PureMockLogger a = PureMockLogger (State [LogMessage] a)
  deriving (Functor, Applicative, Monad)

instance MonadLogger PureMockLogger where
  log logMessage = PureMockLogger $ modify' (logMessage :)
  ambientLogProperties _ a = a -- unneeded as yet, so haven't provided implementation

runPureMockLogger :: PureMockLogger a -> (a, [LogMessage])
runPureMockLogger (PureMockLogger s) = runState s []

execPureMockLogger :: PureMockLogger a -> [LogMessage]
execPureMockLogger (PureMockLogger s) = execState s []

evalPureMockLogger :: PureMockLogger a -> a
evalPureMockLogger (PureMockLogger s) = evalState s []

-- Assertion function that checks all the properties of a LogMessage
-- but does not look at the log message, since that's a human readable
-- bit of text that's not particularly important to test
-- Using the standard Eq for LogMessage would check the message,
-- which is why we used this function instead.
logShouldBe :: LogMessage -> LogLevel -> Text -> Object -> Expectation
logShouldBe LogMessage{..} level type' properties = do
  _lmLevel `shouldBe` level
  _lmType `shouldBe` type'
  _lmProperties `shouldBe` properties

logsShouldBe :: [LogMessage] -> [(LogLevel, Text, Object)] -> Expectation
logsShouldBe logs expectedValues
  | length logs == length expectedValues
  = zipWithM_
      (\logMessage (level, type', properties) -> (logMessage `logShouldBe`) level type' properties)
      logs
      expectedValues
  | otherwise = error $ "Logs length incorrect. Expected " <> show (length expectedValues)
                      <> ", Actual " <> show (length logs)

-- | Tests that the expected properties exist and have the expected values.
-- Additional properties in the Object are ignored. This is good for testing a subset of
-- properties in a LogMessage.
logPropsContain :: Object -> Object -> Expectation
logPropsContain actualProps expectedProps =
  let actualPropsSubset = actualProps `intersection` expectedProps
  in actualPropsSubset `shouldBe` expectedProps
