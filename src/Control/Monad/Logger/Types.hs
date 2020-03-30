{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Logger.Types where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson (Object, ToJSON(..), object, pairs, (.=))
import           Data.Text (Text)
import           GHC.Generics (Generic)

data LogLevel =
    LogDebug
  | LogInformation
  | LogWarning
  | LogError
  | LogFatal
  deriving (Eq, Show, Generic, ToJSON)

data LogMessage =
  LogMessage
    { _lmLevel      :: LogLevel
    , _lmType       :: Text
    , _lmMessage    :: Text
    , _lmProperties :: Object }
  deriving (Eq, Show, Generic)

makeLenses ''LogMessage

instance ToJSON LogMessage where
  toJSON LogMessage{..} = object
    [ "level"      .= _lmLevel
    , "type"       .= _lmType
    , "message"    .= _lmMessage
    , "properties" .= _lmProperties ]

  toEncoding LogMessage{..} = pairs
    (  "type"       .= _lmType
    <> "message"    .= _lmMessage
    <> "level"      .= _lmLevel
    <> "properties" .= _lmProperties )
