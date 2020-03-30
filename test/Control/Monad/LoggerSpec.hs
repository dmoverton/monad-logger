{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.LoggerSpec (spec) where

import           Control.Exception (ErrorCall(..), Exception)
import           Control.Exception.Safe (handleAny)
import           Control.Monad ((<=<))
import           Control.Monad.Catch (throwM)
import           Control.Monad.Identity (runIdentity)
import           Control.Monad.Thyme.Mock
import           Control.Monad.Writer (MonadWriter, Writer, WriterT, runWriterT, tell)
import           Control.Retry (RetryStatus(..))
import           Data.Aeson (Object, Value(..), (.=))
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import           Data.Text1 (fromText1)
import           Data.Thyme (UTCTime, defaultTimeLocale, readTime)
import           Data.Typeable (Typeable)
import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck.Instances ()

import           Control.Monad.Logger
import           Control.Monad.Logger.Mock (execMockLoggerT)

spec :: Spec
spec = describe "Logger" $ do
  it "logInfo creates Information log" $ do
    let logs = execTestLogger mempty $ logInfo "Type" "Message"
    logs `shouldBe` [ LogMessage LogInformation "Type" "Message" mempty ]

  it "logInfo' creates Information log" $ do
    let logs = execTestLogger mempty $ logInfo' "Type" "Message" testProperties
    logs `shouldBe` [ LogMessage LogInformation "Type" "Message" testProperties ]

  it "logWarn creates Warning log" $ do
    let logs = execTestLogger mempty $ logWarn "Type" "Message"
    logs `shouldBe` [ LogMessage LogWarning "Type" "Message" mempty ]

  it "logWarn' creates Warning log" $ do
    let logs = execTestLogger mempty $ logWarn' "Type" "Message" testProperties
    logs `shouldBe` [ LogMessage LogWarning "Type" "Message" testProperties ]

  it "logError creates Error log" $ do
    let logs = execTestLogger mempty $ logError "Type" "Message"
    logs `shouldBe` [ LogMessage LogError "Type" "Message" mempty ]

  it "logError' creates Error log" $ do
    let logs = execTestLogger mempty $ logError' "Type" "Message" testProperties
    logs `shouldBe` [ LogMessage LogError "Type" "Message" testProperties ]

  context "logPrePost'" $ do

    it "logPrePost logs before and after" $ do
      let Right startTime  = fromText1 "2016-12-09T12:00:00Z"
          Right finishTime = fromText1 "2016-12-09T12:00:24Z"
      let result = "test" :: String
      let thingToLogAround = do
            logInfo' "Middle" "In the middle" testProperties
            return result
      let getProperties a = HM.fromList [ "Result" .= a ]

      let (actualResult, logs) =
            runTestLogger mempty $
              runMockTimeT' (startTime :| [finishTime]) $
                logPrePost "PrePost" "PrePost Test" getProperties thingToLogAround

      let expectedPreProperties = HM.fromList
            [ "startedAt" .= String "2016-12-09T12:00:00Z" ]
      let expectedPostProperties = HM.fromList
            [ "Result"    .= result
            , "finishedAt" .= String "2016-12-09T12:00:24Z"
            , "timeTakenInSeconds" .= Number 24 ]
      logs `shouldBe`
        [ LogMessage LogInformation "PrePostStarted" "PrePost Test started" expectedPreProperties,
          LogMessage LogInformation "Middle" "In the middle" testProperties,
          LogMessage LogInformation "PrePostFinished" "PrePost Test finished" expectedPostProperties ]
      actualResult `shouldBe` result

    it "Should correctly time function call" $ do
      let Right startTime  = fromText1 "2016-12-09T12:00:00Z"
          Right finishTime = fromText1 "2016-12-09T12:00:24Z"
      [_, LogMessage{..}] <-
        execTestLoggerT mempty $
          runMockTimeT' (startTime :| [finishTime]) $
            logPrePost' LogInformation "Test" "Test" (const mempty) (pure ())
      HM.lookup "timeTakenInSeconds" _lmProperties `shouldBe` Just (Number 24)

  context "logPrePostAndAmbient'" $ do

    it "Should correctly time function call" $ do
      let Right startTime  = fromText1 "2016-12-09T12:00:00Z"
          Right finishTime = fromText1 "2016-12-09T12:00:24Z"
      [_, LogMessage{..}] <-
        execTestLoggerT mempty $
          runMockTimeT' (startTime :| [finishTime]) $
            logPrePostAndAmbient' LogInformation "Test" "Test" mempty (pure ())
      HM.lookup "timeTakenInSeconds" _lmProperties `shouldBe` Just (Number 24)

  context "logAndRethrow" $ do

    it "Should throw exception" $
      let result = execMockLoggerT $ logAndRethrow makeUnhandledExceptionLog $ throwM TestException
      in result `shouldThrow` testException

    it "Should log exception" $ do
      [LogMessage{..}] <-
        execMockLoggerT $ handleAny (const $ pure ()) $
          logAndRethrow makeUnhandledExceptionLog $ throwM TestException
      _lmType `shouldBe` "UnhandledException"
      HM.lookup "exception" _lmProperties `shouldBe` Just  "TestException"

  context "Initial ambient properties" $ do

    it "Initial ambient properties are logged" $ do
      let initialAmbientProps = HM.fromList [
                "app"     .= String "Importer"
              , "version" .= String "1.2.3.4"
            ]
          logs = execTestLogger initialAmbientProps $ logInfo "Type" "Message"
      logs `shouldBe` [ LogMessage LogInformation "Type" "Message" initialAmbientProps ]

    it "Initial ambient log properties are overridden by actual log properties" $ do
      let logs = execTestLogger testAmbientProperties $ logInfo' "Type" "Message" testProperties
      logs `shouldBe` [ LogMessage LogInformation "Type" "Message" (testProperties <> testAmbientProperties) ]

    it "Initial ambient log properties are overridden by inner log properties" $ do
      let innerAmbientProperties = HM.fromList [ "SuchAmbient" .= String "Woow",
                                                      "Test2" .= String "Value2" ]
      let logs = execTestLogger testAmbientProperties
                     . appendAmbientLogProperties innerAmbientProperties $
                  logInfo' "Type" "Message" testProperties
      logs `shouldBe` [ LogMessage LogInformation "Type" "Message" (testProperties <> innerAmbientProperties <> testAmbientProperties) ]

  context "appendAmbientLogProperties" $ do
    it "ambient log properties are overridden by actual log properties" $ do
      let logs = execTestLogger mempty . appendAmbientLogProperties testAmbientProperties $
                  logInfo' "Type" "Message" testProperties
      logs `shouldBe` [ LogMessage LogInformation "Type" "Message" (testProperties <> testAmbientProperties) ]

    it "inner ambient log properties override outer log properties" $ do
      let outerAmbientProperties = HM.fromList [ "SuchAmbient" .= String "Woow",
                                                      "Test2" .= String "Value2" ]
      let logs = execTestLogger mempty
                     . appendAmbientLogProperties outerAmbientProperties
                     . appendAmbientLogProperties testAmbientProperties $
                  logInfo' "Type" "Message" testProperties
      logs `shouldBe` [ LogMessage LogInformation "Type" "Message" (testProperties <> testAmbientProperties <> outerAmbientProperties) ]

  context "enrichLogWithTimestamp" $

    it "adds timestamp to logs" $
      let testLog = LogMessage LogInformation "SuchLog" "SuchWow" testProperties
          expectedProperties = HM.fromList [ "Test" .= String "Value", "utcTimestamp" .= String "2015-01-20T00:00:00Z"]
          LogMessage{..} = enrichLogWithTimestamp today testLog
      in _lmProperties `shouldBe` expectedProperties

  context "makeUnhandledExceptionLog" $

    it "logs exception show string" $ do
      let exception = ErrorCall "We're fine. We're all fine here now, thank you. How are you?"

      (makeUnhandledExceptionLog exception `logShouldBe`) LogFatal "UnhandledException" $
        toObject [
          "exception" .= show exception
        ]

  context "makeCaughtExceptionLog" $

    it "logs exception show string" $ do
      let exception = ErrorCall "We're fine. We're all fine here now, thank you. How are you?"

      (makeCaughtExceptionLog exception `logShouldBe`) LogError "CaughtException" $
        toObject [
          "exception" .= show exception
        ]

  context "makeRecoveringExceptionLog" $

    it "logs exception and current retry status" $ do
      let exception = ErrorCall "We're fine. We're all fine here now, thank you. How are you?"
          status = RetryStatus 3 1500000 $ Just 50000

      (makeRecoveringExceptionLog status exception `logShouldBe`) LogWarning "RecoveringException" $
        toObject [
            "exception"       .= show exception
          , "iteration"       .= Number 3
          , "cumulativeDelay" .= toObject [ "value" .= Number 1.5, "unit" .= String "s" ]
          , "previousDelay"   .= Number 50000
        ]

today :: UTCTime
today = readTime defaultTimeLocale "%Y-%m-%d" "2015-01-20"

testProperties :: Object
testProperties = HM.fromList [ "Test" .= String "Value" ]

testAmbientProperties :: Object
testAmbientProperties = HM.fromList [ "Test"        .= String "AmbientValue"
                                    , "SuchAmbient" .= String "Very Subtle, Much Background, Wow" ]

-- Assertion function that checks all the properties of a LogMessage
-- but does not look at the log message, since that's a human readable
-- bit of text that's not particularly important to test
-- Using the standard Eq for LogMessage would check the message,
-- which is why we used this function instead.
logShouldBe :: LogMessage -> LogLevel -> Text -> Object -> Expectation
logShouldBe LogMessage{..} lvl lType props = do
  _lmLevel `shouldBe` lvl
  _lmType `shouldBe` lType
  _lmProperties `shouldBe` props

newtype TestLoggerT m a = TestLoggerT { unwrapLoggerT :: LoggerT m a }
  deriving (Functor, Applicative, Monad)

-- This instance requires UndecidableInstances, but that seems okay
-- since so does any instance of MonadWriter for other monad transformers
-- where the inner monad is a MonadWriter (eg MonadWriter for ReaderT
-- in mtl uses UndecideableInstances)
instance (MonadWriter [LogMessage] m) => MonadLogger (TestLoggerT m) where
  log = TestLoggerT . tell . pure <=< TestLoggerT . enrichLogMessage
  ambientLogProperties fn = TestLoggerT . localAmbientLogProperties fn . unwrapLoggerT

runTestLogger :: Object -> TestLoggerT (Writer [LogMessage]) a -> (a, [LogMessage])
runTestLogger ambientProps = runIdentity . runTestLoggerT ambientProps

execTestLogger :: Object -> TestLoggerT (Writer [LogMessage]) a -> [LogMessage]
execTestLogger ambientProps = runIdentity . execTestLoggerT ambientProps

runTestLoggerT :: Monad m => Object -> TestLoggerT (WriterT [LogMessage] m) a -> m (a, [LogMessage])
runTestLoggerT ambientProps = runWriterT . runLoggerT ambientProps . unwrapLoggerT

execTestLoggerT :: Monad m => Object -> TestLoggerT (WriterT [LogMessage] m) a -> m [LogMessage]
execTestLoggerT ambientProps = fmap snd . runTestLoggerT ambientProps

testException :: Selector TestException
testException = const True

data TestException = TestException
  deriving (Show, Eq, Typeable)

instance Exception TestException
