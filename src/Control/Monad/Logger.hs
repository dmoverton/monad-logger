{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Logger
    ( module Control.Monad.Logger.Types
    , MonadLogger(..)
    , appendAmbientLogProperties
    , LoggerT
    , localAmbientLogProperties
    , enrichLogMessage
    , enrichLogWithTimestamp
    , runLoggerT

    , logDebug
    , logDebug'
    , logInfo
    , logInfo'
    , logWarn
    , logWarn'
    , logError
    , logError'
    , logFatal
    , logFatal'
    , logPrePost
    , logPrePost'
    , logPrePostAndAmbient
    , logPrePostAndAmbient'
    , logAndRethrow
    , handleAndLogExceptions
    , handleAndLogExceptions'
    , catchTerminationSignalAndLog
    , logTerminationSignalWarning

    , makeDebugLog
    , makeDebugLog'
    , makeInfoLog
    , makeInfoLog'
    , makeWarnLog
    , makeWarnLog'
    , makeErrorLog
    , makeErrorLog'
    , makeFatalLog
    , makeFatalLog'
    , makeUnhandledExceptionLog
    , makeCaughtExceptionLog
    , makeRecoveringExceptionLog
    , makeTerminationSignalLog
    , makeTerminationSignalWarningLog

    , logMessageToJson
    , logMessageToJsonBS
    , logMessageToJsonBuilder

    , toObject
    , toArray
    ) where

import           Control.Exception.Base (Exception, SomeException)
import           Control.Exception.Safe (handleAny)
import           Control.Lens ((%~), (&), (<>~))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadCatch(..), MonadThrow(..), handle)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Logger.Types
-- import           Control.Monad.Thyme (MonadTime(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Control
                  (ComposeSt, MonadBaseControl(..), MonadTransControl(..), StT,
                  defaultLiftBaseWith, defaultLiftWith, defaultRestoreM, defaultRestoreT)
import           Control.Monad.Trans.Free.Church (FT, hoistFT)
import           Control.Monad.Trans.Resource (ResourceT, transResourceT)
import           Control.Monad.Writer (MonadWriter)
import           Control.Retry (RetryStatus(..))
import           Data.Aeson (Object, Value(..), encode, fromEncoding, object, toEncoding, (.=))
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Aeson.Types (Pair)
import           Data.AffineSpace ((.-.))
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Conduit.Internal (ConduitT(..), Pipe(..))
import           Data.Function (on)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Thyme (UTCTime, getCurrentTime, toSeconds)
import qualified Data.Vector as Vector
import           Prelude hiding (log)
import           System.Exit (exitFailure)
import           TextShow (showt)

import           Control.Exception.Censor (censorAndDisplayException)

class Monad m => MonadLogger m where
  log :: LogMessage -> m ()
  ambientLogProperties :: (Object -> Object) -> m a -> m a

appendAmbientLogProperties :: MonadLogger m => Object -> m a -> m a
appendAmbientLogProperties props = ambientLogProperties (props <>)

newtype LoggerContext =
  LoggerContext
    { _lcAmbientProperties :: Object }

makeLenses ''LoggerContext

newtype LoggerT m a =
  LoggerT
    { _unwrapReaderT :: ReaderT LoggerContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow
           , MonadError e, MonadCatch, MonadWriter w, MonadBase b)

-- This instance and MonadBaseControl require UndecidableInstances to compile.
-- The implementation of these has been adapted from the implementation used
-- by Amazonka's AWST' type, which does a similar newtype over ReaderT thing.
-- Be careful when editing these, as if you manage to get the compiler to
-- infinite loop during type checking (due to using UndecidableInstances)
-- it will consume lots of memory (8GB) very quickly before crashing eventually.
instance MonadTransControl LoggerT where
  type StT LoggerT a = StT (ReaderT LoggerContext) a

  liftWith = defaultLiftWith LoggerT _unwrapReaderT
  restoreT = defaultRestoreT LoggerT

instance MonadBaseControl b m => MonadBaseControl b (LoggerT m) where
  type StM (LoggerT m) a = ComposeSt LoggerT m a

  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadUnliftIO m => MonadUnliftIO (LoggerT m) where
  askUnliftIO = LoggerT $ (\(UnliftIO f) -> UnliftIO (f . _unwrapReaderT)) <$> askUnliftIO

instance PrimMonad m => PrimMonad (LoggerT m) where
  type PrimState (LoggerT m) = PrimState m
  primitive f = LoggerT $ primitive f

instance {-# OVERLAPPABLE #-} (MonadTrans t, MFunctor t, Monad (t m), MonadLogger m) => MonadLogger (t m) where
  log = lift . log
  ambientLogProperties fn = hoist $ ambientLogProperties fn

instance MonadLogger m => MonadLogger (ResourceT m) where
  log = lift . log
  ambientLogProperties fn = transResourceT $ ambientLogProperties fn

instance MonadLogger m => MonadLogger (ConduitT i o m) where
  log = lift . log
  -- See https://github.com/snoyberg/conduit/blob/master/conduit/src/Data/Conduit/Internal/Conduit.hs#L168
  ambientLogProperties f (ConduitT c0) = ConduitT $ \rest ->
    let go (HaveOutput p o) = HaveOutput (go p) o
        go (NeedInput p c)  = NeedInput (\i -> go (p i)) (\u -> go (c u))
        go (Done x)         = rest x
        go (PipeM mp)       = PipeM (fmap go $ ambientLogProperties f mp)
        go (Leftover p i)   = Leftover (go p) i
     in go (c0 Done)

instance {-# OVERLAPPING #-} MonadIO m => MonadLogger (LoggerT m) where
  log logMessage = do
    utcTime <- liftIO getCurrentTime
    enrichedLogMessage <- enrichLogMessage $ enrichLogWithTimestamp utcTime logMessage
    liftIO . BL8.putStrLn . logMessageToJsonBS $ enrichedLogMessage
  ambientLogProperties = localAmbientLogProperties

instance {-# OVERLAPPING #-} MonadLogger m => MonadLogger (FT f m) where
  log = lift . log
  ambientLogProperties fn = hoistFT $ ambientLogProperties fn

enrichLogWithTimestamp :: UTCTime -> LogMessage -> LogMessage
enrichLogWithTimestamp utcTime logMessage =
  -- ensure this property name does not get parsed by Fluent as the time for the log message
  let timestampProperties = HashMap.fromList [ "utcTimestamp" .= utcTime ]
  in logMessage & lmProperties <>~ timestampProperties

localAmbientLogProperties :: Monad m => (Object -> Object) -> LoggerT m a -> LoggerT m a
localAmbientLogProperties fn = localLoggerContext (lcAmbientProperties %~ fn)

enrichLogMessage :: Monad m => LogMessage -> LoggerT m LogMessage
enrichLogMessage logMessage = do
  ambientLogProps <- _lcAmbientProperties <$> askLoggerContext
  return $ lmProperties <>~ ambientLogProps $ logMessage

askLoggerContext :: Monad m => LoggerT m LoggerContext
askLoggerContext = LoggerT ask

localLoggerContext :: Monad m => (LoggerContext -> LoggerContext) -> LoggerT m a -> LoggerT m a
localLoggerContext fn = LoggerT . local fn . _unwrapReaderT

runLoggerT :: Object -> LoggerT m a -> m a
runLoggerT ambientProps (LoggerT r) = runReaderT r $ LoggerContext ambientProps

logDebug :: MonadLogger m => Text -> Text -> m ()
logDebug l t = log $ makeDebugLog l t

logDebug' :: MonadLogger m => Text -> Text -> Object -> m ()
logDebug' l t p = log $ makeDebugLog' l t p

logInfo :: MonadLogger m => Text -> Text -> m ()
logInfo l t = log $ makeInfoLog l t

logInfo' :: MonadLogger m => Text -> Text -> Object -> m ()
logInfo' l t p = log $ makeInfoLog' l t p

logWarn :: MonadLogger m => Text -> Text -> m ()
logWarn l t = log $ makeWarnLog l t

logWarn' :: MonadLogger m => Text -> Text -> Object -> m ()
logWarn' l t p = log $ makeWarnLog' l t p

logError :: MonadLogger m => Text -> Text -> m ()
logError l t = log $ makeErrorLog l t

logError' :: MonadLogger m => Text -> Text -> Object -> m ()
logError' l t p = log $ makeErrorLog' l t p

logFatal :: MonadLogger m => Text -> Text -> m ()
logFatal l t = log $ makeFatalLog l t

logFatal' :: MonadLogger m => Text -> Text -> Object -> m ()
logFatal' l t p = log $ makeFatalLog' l t p

logPrePost :: (MonadLogger m, MonadTime m) => Text -> Text -> (a -> Object) -> m a -> m a
logPrePost = logPrePost' LogInformation

logPrePost' :: forall m a.
  (MonadLogger m, MonadTime m)
  => LogLevel -> Text -> Text -> (a -> Object) -> m a -> m a
logPrePost' logLevel logType label toLogProperties ma = do
  startTime <- currentTime
  log $ startedLogMessage startTime
  a <- ma
  finishedTime <- currentTime
  log $ finishedLogMessage startTime finishedTime a
  return a
  where
    startedLogMessage :: UTCTime -> LogMessage
    startedLogMessage startTime = do
      let logProperties = HashMap.fromList [ "startedAt" .= startTime ]
      LogMessage logLevel (logType <> "Started") (label <> " started") logProperties

    finishedLogMessage :: UTCTime -> UTCTime -> a -> LogMessage
    finishedLogMessage startTime finishedTime a = do
      let timeTaken = finishedTime .-. startTime
          logProperties =
            toLogProperties a <>
            HashMap.fromList [ "finishedAt" .= finishedTime, "timeTakenInSeconds" .= toSeconds @_ @Double timeTaken ]
      LogMessage logLevel (logType <> "Finished") (label <> " finished") logProperties

logPrePostAndAmbient :: (MonadLogger m, MonadTime m) => Text -> Text -> Object -> m a -> m a
logPrePostAndAmbient = logPrePostAndAmbient' LogInformation

logPrePostAndAmbient' :: (MonadLogger m, MonadTime m) => LogLevel -> Text -> Text -> Object -> m a -> m a
logPrePostAndAmbient' logLevel logType label props =
  appendAmbientLogProperties props . logPrePost' logLevel logType label (const mempty)

logAndRethrow :: (MonadCatch m, MonadLogger m) => (SomeException -> LogMessage) -> m a -> m a
logAndRethrow makeLog =
  handleAny $ \exception -> do
    log $ makeLog exception
    throwM exception

handleAndLogExceptions :: (MonadIO m, MonadCatch m, MonadLogger m) => m a -> m a
handleAndLogExceptions = handleAndLogExceptions' makeUnhandledExceptionLog

handleAndLogExceptions' ::
  (MonadIO m, MonadCatch m, MonadLogger m) => (SomeException -> LogMessage) -> m a -> m a
handleAndLogExceptions' makeExceptionLog =
  handle (\e -> (log . makeExceptionLog) e >> liftIO exitFailure)

catchTerminationSignalAndLog :: (MonadCatch m, MonadLogger m) => m () -> m ()
catchTerminationSignalAndLog action = action `catch` (log . makeTerminationSignalLog)

logTerminationSignalWarning :: (MonadCatch m, MonadLogger m) => Text -> m a -> m a
logTerminationSignalWarning warningText action = action `catch` \signal -> do
  log $ makeTerminationSignalWarningLog warningText signal
  throwM signal

makeDebugLog :: Text -> Text -> LogMessage
makeDebugLog l t = makeDebugLog' l t HashMap.empty

makeDebugLog' :: Text -> Text -> Object -> LogMessage
makeDebugLog' = LogMessage LogDebug

makeInfoLog :: Text -> Text -> LogMessage
makeInfoLog l t = makeInfoLog' l t HashMap.empty

makeInfoLog' :: Text -> Text -> Object -> LogMessage
makeInfoLog' = LogMessage LogInformation

makeWarnLog :: Text -> Text -> LogMessage
makeWarnLog l t = makeWarnLog' l t HashMap.empty

makeWarnLog' :: Text -> Text -> Object -> LogMessage
makeWarnLog' = LogMessage LogWarning

makeErrorLog :: Text -> Text -> LogMessage
makeErrorLog l t = makeErrorLog' l t HashMap.empty

makeErrorLog' :: Text -> Text -> Object -> LogMessage
makeErrorLog' = LogMessage LogError

makeFatalLog :: Text -> Text -> LogMessage
makeFatalLog l t = makeFatalLog' l t HashMap.empty

makeFatalLog' :: Text -> Text -> Object -> LogMessage
makeFatalLog' = LogMessage LogFatal

makeUnhandledExceptionLog :: Exception e => e -> LogMessage
makeUnhandledExceptionLog e =
  LogMessage LogFatal "UnhandledException" "Unhandled Exception" $
    toObject [
      "exception" .= censorAndDisplayException e
    ]

makeCaughtExceptionLog :: Exception e => e -> LogMessage
makeCaughtExceptionLog e =
  LogMessage LogError "CaughtException" "Caught Exception" $
    toObject [
      "exception" .= censorAndDisplayException e
    ]

makeRecoveringExceptionLog :: Exception e => RetryStatus -> e -> LogMessage
makeRecoveringExceptionLog RetryStatus{..} e =
  LogMessage LogWarning "RecoveringException" "Recovering Exception" $
    toObject [
        "exception"       .= censorAndDisplayException e
      , "iteration"       .= rsIterNumber
      , "cumulativeDelay" .= object [ "value" .= cumulativeDelayInSeconds, "unit" .= String "s"]
      , "previousDelay"   .= rsPreviousDelay
    ]
  where
    -- | Converts cumulative delay from microseconds to seconds
    cumulativeDelayInSeconds :: Double
    cumulativeDelayInSeconds = rsCumulativeDelay `divide` 1000000

    divide :: Int -> Int -> Double
    divide = (/) `on` fromIntegral

logMessageToJson :: LogMessage -> Text
logMessageToJson =  TL.toStrict . encodeToLazyText

logMessageToJsonBS :: LogMessage -> BL8.ByteString
logMessageToJsonBS = encode

logMessageToJsonBuilder :: LogMessage -> BS.Builder
logMessageToJsonBuilder = fromEncoding . toEncoding

toObject :: [Pair] -> Object
toObject = HashMap.fromList

toArray :: [[Pair]] -> Value
toArray = Array . Vector.fromList . fmap object
