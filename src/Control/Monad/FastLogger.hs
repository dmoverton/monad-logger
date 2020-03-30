{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.FastLogger
    ( FastLoggerT
    , runFastLoggerT
    , runFastLoggerTWithBuffer
    , defaultFastLoggerTBufSize
    , lcLoggerSet
    , lcAmbientProperties
    , module Control.Monad.Logger
    ) where

import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Lens ((%~), (&), (<>~))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadCatch(..), MonadMask(..), MonadThrow(..), finally)
import           Control.Monad.Except (MonadError)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..))
import           Control.Monad.Logger hiding (LoggerT, enrichLogMessage)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Control
                  (ComposeSt, MonadBaseControl(..), MonadTransControl(..), StT,
                  defaultLiftBaseWith, defaultLiftWith, defaultRestoreM, defaultRestoreT)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Writer (MonadWriter)
import           Data.Aeson (Object, (.=))
import qualified Data.HashMap.Strict as HashMap
import           Data.Thyme (UTCTime, getCurrentTime)
import           System.Log.FastLogger
                  (BufSize, LoggerSet, defaultBufSize, flushLogStr, newStdoutLoggerSet,
                  pushLogStrLn, toLogStr)

data LoggerContext =
  LoggerContext
    { _lcLoggerSet         :: LoggerSet
    , _lcAmbientProperties :: Object }

makeLenses 'LoggerContext

newtype FastLoggerT m a =
  FastLoggerT
    { _runFastLoggerT :: ReaderT LoggerContext m a }
    deriving ( Functor, Applicative, Monad, MonadReader LoggerContext, MonadIO
             , MonadTrans, MonadThrow , MonadError e, MonadCatch, MonadWriter w
             , MonadBase b, MonadMask, MonadResource)

instance MonadIO m => MonadLogger (FastLoggerT m) where
  log :: LogMessage -> FastLoggerT m ()
  log logMessage = do
    LoggerContext{..} <- ask
    (utcTime, threadId) <- liftIO $ (,) <$> getCurrentTime <*> myThreadId
    enrichedLogMessage <- enrichLogMessage $ enrichLogWithTimestampAndThreadId utcTime threadId logMessage
    liftIO . pushLogStrLn _lcLoggerSet . toLogStr . logMessageToJsonBuilder $ enrichedLogMessage

  ambientLogProperties :: (Object -> Object) -> FastLoggerT m a -> FastLoggerT m a
  ambientLogProperties f = local (lcAmbientProperties %~ f)

instance PrimMonad m => PrimMonad (FastLoggerT m) where
  type PrimState (FastLoggerT m) = PrimState m
  primitive f = FastLoggerT $ primitive f

instance MonadUnliftIO m => MonadUnliftIO (FastLoggerT m) where
  askUnliftIO = FastLoggerT $ (\(UnliftIO f) -> UnliftIO (f . _runFastLoggerT)) <$> askUnliftIO

instance MonadTransControl FastLoggerT where
  type StT FastLoggerT a = StT (ReaderT LoggerContext) a

  liftWith = defaultLiftWith FastLoggerT _runFastLoggerT
  restoreT = defaultRestoreT FastLoggerT

instance MonadBaseControl b m => MonadBaseControl b (FastLoggerT m) where
  type StM (FastLoggerT m) a = ComposeSt FastLoggerT m a

  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

enrichLogMessage :: Monad m => LogMessage -> FastLoggerT m LogMessage
enrichLogMessage logMessage = do
  ambientLogProps <- asks _lcAmbientProperties
  return $ lmProperties <>~ ambientLogProps $ logMessage

enrichLogWithTimestampAndThreadId :: UTCTime -> ThreadId -> LogMessage -> LogMessage
enrichLogWithTimestampAndThreadId utcTime threadId logMessage =
  -- ensure utcTimestamp property name does not get parsed by Fluent as the time for the log message
  let timestampProperties = HashMap.fromList [ "utcTimestamp" .= utcTime, "threadId" .= show threadId ]
  in logMessage & lmProperties <>~ timestampProperties

runFastLoggerT :: (MonadIO m, MonadMask m) => LoggerSet -> Object -> FastLoggerT m a -> m a
runFastLoggerT logSet ambientProps (FastLoggerT r) =
  runReaderT r (LoggerContext logSet ambientProps) `finally` (liftIO $ flushLogStr logSet)

runFastLoggerTWithBuffer :: (MonadIO m, MonadMask m) => BufSize -> Object -> FastLoggerT m a -> m a
runFastLoggerTWithBuffer bufSize logProperties ma = do
  loggerSet <- liftIO $ newStdoutLoggerSet bufSize
  runFastLoggerT loggerSet logProperties ma

defaultFastLoggerTBufSize :: BufSize
defaultFastLoggerTBufSize = defaultBufSize
