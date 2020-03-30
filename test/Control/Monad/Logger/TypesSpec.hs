module Control.Monad.Logger.TypesSpec (spec) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           Test.Hspec

import           Control.Monad.Logger.Types

spec :: Spec
spec = describe "Types" $

  context "LogMessage" $

    it "toJSON remove prefixes" $
      let logMessage =
            LogMessage LogInformation "AddAttempt" "Some message" $ HashMap.fromList ["thing" .= Number 123]
      in toJSON logMessage `shouldBe`
          object [ "level"      .= String "LogInformation"
                 , "type"       .= String "AddAttempt"
                 , "message"    .= String "Some message"
                 , "properties" .= object ["thing" .= Number 123]]
