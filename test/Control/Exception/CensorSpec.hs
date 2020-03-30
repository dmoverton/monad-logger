module Control.Exception.CensorSpec (spec) where

import           Control.Exception (Exception(..))
import           Control.Lens ((&), (.~))
import           Control.Monad (forM_)
import qualified Data.ByteString.Lazy as L
import           Data.Either.Combinators (fromRight')
import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Exts (fromList)
import           Network.AWS.Data.Text (fromText)
import           Network.AWS.Types (Error(..), ErrorCode(..), ServiceError(..), serviceHeaders)
import           Network.HTTP.Client
                  (HttpException(..), HttpExceptionContent(..), defaultRequest, requestHeaders,
                  responseHeaders)
import           Network.HTTP.Client.Internal (Response(..), ResponseClose(..), createCookieJar)
import           Network.HTTP.Types.Header (Header)
import           Network.HTTP.Types.Status (status500)
import           Network.HTTP.Types.Version (http11)
import           Servant.Client (ServantError(..))
import qualified Servant.Client as Servant
import           Test.Hspec

import           Control.Exception.Censor

spec :: Spec
spec = parallel $ do

  context "censorHeaders" $ do

    it "Should censor headers not on whitelist" $ do
      let headers = [ ("X-Amz-Security-Token", "Super secret")
                    , ("Authorization", "I can pwn you with this") ]
          censored = censorHeaders headers
      censored `shouldSatisfy` all (\(_, headerValue) -> headerValue == "**REDACTED**")

    it "Should censor headers not on whitelist regardless of case" $ do
      let headers = [ ("X-AMZ-SECURITY-TOKEN", "Super secret")
                    , ("authorization", "I can pwn you with this") ]
          censored = censorHeaders headers
      censored `shouldSatisfy` all (\(_, headerValue) -> headerValue == "**REDACTED**")

    it "Should retain multiple censored headers with the same name" $ do
      let headers = [ ("X-Amz-Security-Token", "Super secret")
                    , ("X-Amz-Security-Token", "I can pwn you with this") ]
          censored = censorHeaders headers
          headerNames = fst <$> headers
          censoredHeaderNames = fst <$> censored
      censoredHeaderNames `shouldBe` headerNames

    it "Should not censor headers on whitelist" $ do
      let headers = (, "UNCENSORED") <$> Set.toList headerNameWhitelist
      censorHeaders headers `shouldBe` headers

    it "Should not censor headers on whitelist regardless of case" $ do
      let headers = [("x-amz-request-id", "UNCENSORED")]
      censorHeaders headers `shouldBe` headers

    it "Should retain multiple whitelisted headers with the same name" $ do
      let headerNames = Set.toList headerNameWhitelist ++ Set.toList headerNameWhitelist
          headers = (, "UNCENSORED") <$> headerNames
          censored = censorHeaders headers
          censoredHeaderNames = fst <$> censored
      censoredHeaderNames `shouldBe` headerNames

  context "censorBearerToken" $ do

    it "Should censor bearer token" $
      forM_
        [ ("Bearer UNCENSORED", "Bearer **REDACTED**")
        , ("Bearer UNCENSORED ", "Bearer **REDACTED** ")
        , ("\"Bearer UNCENSORED\"", "\"Bearer **REDACTED**\"")
        , ("\\\"Bearer UNCENSORED\\\"", "\\\"Bearer **REDACTED**\\\"")
        , ("\\\"Bearer UNC-E_n.S00R3D\\\"", "\\\"Bearer **REDACTED**\\\"")
        , ("bearer UNCENSORED", "Bearer **REDACTED**")
        , ("bearer UNCENSORED ", "Bearer **REDACTED** ")
        , ("\"bearer UNCENSORED\"", "\"Bearer **REDACTED**\"")
        , ("\\\"bearer UNCENSORED\\\"", "\\\"Bearer **REDACTED**\\\"")
        , ("BEARER UNCENSORED", "Bearer **REDACTED**")
        , ("BEARER UNCENSORED ", "Bearer **REDACTED** ")
        , ("\"BEARER UNCENSORED\"", "\"Bearer **REDACTED**\"")
        , ("\\\"BEARER UNCENSORED\\\"", "\\\"Bearer **REDACTED**\\\"")
        , ("\\\"Bearer UNCENSORED\\\" other text \\\"Bearer UNCENSORED\\\"",
          "\\\"Bearer **REDACTED**\\\" other text \\\"Bearer **REDACTED**\\\"") ]
        $ \(input, expected) -> censorBearerToken (input :: Text) `shouldBe` expected

  context "censorServiceError" $

    it "Should censor headers not on whitelist" $ do
      let censoredServiceError = censorServiceError serviceError
          expectedServiceError = serviceError & serviceHeaders .~ expectedCensoredHeaders
      censoredServiceError `shouldBe` expectedServiceError

  context "censorHttpException" $ do

    it "Should leave InvalidUrlException unchanged" $ do
      let httpException = InvalidUrlException "http://somethingiswrong" "Invalid"
      show (censorHttpException httpException) `shouldBe` show httpException -- no Eq instance

    it "Should censor request headers not on whitelist" $ do
      let request = defaultRequest { requestHeaders = headersToCensor }
          httpException = HttpExceptionRequest request ResponseTimeout
          expectedCensoredRequest = defaultRequest { requestHeaders = expectedCensoredHeaders }
          expectedCensoredHttpException = HttpExceptionRequest expectedCensoredRequest ResponseTimeout
      show (censorHttpException httpException) `shouldBe` show expectedCensoredHttpException -- no Eq instance

    it "Should censor response headers for StatusCodeException" $ do
      let content = StatusCodeException (response ()) "Bearer UNCENSORED"
          httpException = HttpExceptionRequest defaultRequest content
          expectedCensoredResponse = (response ()) { responseHeaders = expectedCensoredHeaders }
          expectedCensoredHttpException = HttpExceptionRequest defaultRequest (StatusCodeException expectedCensoredResponse "Bearer **REDACTED**")
      show (censorHttpException httpException) `shouldBe` show expectedCensoredHttpException -- no Eq instance

    it "Should censor response headers for TooManyRedirects" $ do
      let content = TooManyRedirects
            [ response "body1 \\\"authorization\\\":\\\"Bearer UNCENSORED\\\""
            , response "body2 \\\"authorization\\\":\\\"Bearer UNCENSORED\\\""
            , response "body3 \\\"authorization\\\":\\\"Bearer UNCENSORED\\\"" ]
          httpException = HttpExceptionRequest defaultRequest content
          expectedCensoredResponse :: L.ByteString -> Response L.ByteString
          expectedCensoredResponse body = (response body) { responseHeaders = expectedCensoredHeaders }
          expectedCensoredResponses =
            [ expectedCensoredResponse "body1 \\\"authorization\\\":\\\"Bearer **REDACTED**\\\""
            , expectedCensoredResponse "body2 \\\"authorization\\\":\\\"Bearer **REDACTED**\\\""
            , expectedCensoredResponse "body3 \\\"authorization\\\":\\\"Bearer **REDACTED**\\\"" ]
          expectedCensoredHttpException = HttpExceptionRequest defaultRequest (TooManyRedirects expectedCensoredResponses)
      show (censorHttpException httpException) `shouldBe` show expectedCensoredHttpException -- no Eq instance

  context "censorServantError" $ do

    it "Should censor FailureResponse" $
      let servantError = FailureResponse $ servantResponse "Bearer UNCENSORED" headersToCensor
          expected = FailureResponse $ servantResponse "Bearer **REDACTED**" expectedCensoredHeaders
      in censorServantError servantError `shouldBe` expected

    it "Should censor DecodeFailure" $
      let servantError = DecodeFailure "type" $ servantResponse "Bearer UNCENSORED" headersToCensor
          expected =
            DecodeFailure "type" $ servantResponse "Bearer **REDACTED**" expectedCensoredHeaders
      in censorServantError servantError `shouldBe` expected

    it "Should censor UnsupportedContentType" $
      let servantError =
            UnsupportedContentType "text/plain;charset=UTF-8" $
              servantResponse "Bearer UNCENSORED" headersToCensor
          expected =
            UnsupportedContentType "text/plain;charset=UTF-8" $
              servantResponse "Bearer **REDACTED**" expectedCensoredHeaders
      in censorServantError servantError `shouldBe` expected

    it "Should censor InvalidContentTypeHeader" $
      let servantError =
            InvalidContentTypeHeader $ servantResponse "Bearer UNCENSORED" headersToCensor
          expected =
            InvalidContentTypeHeader $ servantResponse "Bearer **REDACTED**" expectedCensoredHeaders
      in censorServantError servantError `shouldBe` expected

    it "Should censor ConnectionError" $
      let servantError = ConnectionError "Bearer UNCENSORED"
          expected = ConnectionError "Bearer **REDACTED**"
      in censorServantError servantError `shouldBe` expected

  context "censorException" $ do

    it "Should censor ServiceErrors" $ do
      let censoredServiceError = censorException $ ServiceError serviceError
          expectedServiceError = ServiceError $ serviceError & serviceHeaders .~ expectedCensoredHeaders
      show censoredServiceError `shouldBe` show expectedServiceError -- no Eq instance

    it "Should censor HttpExceptions" $ do
      let request = defaultRequest { requestHeaders = headersToCensor }
          httpException = HttpExceptionRequest request ResponseTimeout
          expectedCensoredRequest = defaultRequest { requestHeaders = expectedCensoredHeaders }
          expectedCensoredHttpException = HttpExceptionRequest expectedCensoredRequest ResponseTimeout
      show (censorException httpException) `shouldBe` show expectedCensoredHttpException -- no Eq instance

    it "Should censor ServantErrors" $
      let servantError = FailureResponse $ servantResponse "Bearer UNCENSORED" headersToCensor
          expected = FailureResponse $ servantResponse "Bearer **REDACTED**" expectedCensoredHeaders
      in censorException servantError `shouldBe` expected

    it "Should censor HttpExceptions wrapped in Amazonkas TransportError" $ do
      let request = defaultRequest { requestHeaders = headersToCensor }
          transportError = TransportError $ HttpExceptionRequest request ResponseTimeout
          expectedCensoredRequest = defaultRequest { requestHeaders = expectedCensoredHeaders }
          expectedCensoredTransportError = TransportError $ HttpExceptionRequest expectedCensoredRequest ResponseTimeout
      show (censorException transportError) `shouldBe` show expectedCensoredTransportError -- no Eq instance

    it "Should censor ServiceErrors wrapped in SomeException" $ do
      let censoredServiceError = censorException . toException $ ServiceError serviceError
          expectedServiceError = toException . ServiceError $ serviceError & serviceHeaders .~ expectedCensoredHeaders
      show censoredServiceError `shouldBe` show expectedServiceError -- no Eq instance

    it "Should censor ServiceErrors wrapped in many SomeExceptions" $ do
      let censoredServiceError = censorException . toException . toException . toException $ ServiceError serviceError
          expectedServiceError = toException . ServiceError $ serviceError & serviceHeaders .~ expectedCensoredHeaders
      show censoredServiceError `shouldBe` show expectedServiceError -- no Eq instance

  context "censorAndDisplayException" $ do

    it "Should censor ServiceErrors" $ do
      let output = censorAndDisplayException $ ServiceError serviceError
      output `shouldNotSatisfy` List.isInfixOf "Super secret"
      output `shouldNotSatisfy` List.isInfixOf "I can pwn you with this"

    it "Should censor HttpExceptions" $ do
      let request = defaultRequest { requestHeaders = headersToCensor }
          httpException = HttpExceptionRequest request ResponseTimeout
          output = censorAndDisplayException httpException
      output `shouldNotSatisfy` List.isInfixOf "Super secret"
      output `shouldNotSatisfy` List.isInfixOf "I can pwn you with this"

    it "Should censor ServantErrors" $ do
      let servantError = FailureResponse $ servantResponse "Bearer secret" headersToCensor
          output = censorAndDisplayException servantError
      output `shouldNotSatisfy` List.isInfixOf "Super secret"
      output `shouldNotSatisfy` List.isInfixOf "secret"
      output `shouldNotSatisfy` List.isInfixOf "I can pwn you with this"

    it "Should censor HttpExceptions wrapped in Amazonkas TransportError" $ do
      let request = defaultRequest { requestHeaders = headersToCensor }
          httpException = HttpExceptionRequest request ResponseTimeout
          output = censorAndDisplayException $ TransportError httpException
      output `shouldNotSatisfy` List.isInfixOf "Super secret"
      output `shouldNotSatisfy` List.isInfixOf "I can pwn you with this"

    it "Should censor ServiceErrors wrapped in SomeException" $ do
      let output = censorAndDisplayException . toException $ ServiceError serviceError
      output `shouldNotSatisfy` List.isInfixOf "Super secret"
      output `shouldNotSatisfy` List.isInfixOf "I can pwn you with this"

    it "Should censor ServiceErrors wrapped in many SomeExceptions" $ do
      let output = censorAndDisplayException . toException . toException . toException $ ServiceError serviceError
      output `shouldNotSatisfy` List.isInfixOf "Super secret"
      output `shouldNotSatisfy` List.isInfixOf "I can pwn you with this"

response :: body -> Response body
response body = Response
  { responseStatus    = status500
  , responseVersion   = http11
  , responseHeaders   = headersToCensor
  , responseBody      = body
  , responseCookieJar = createCookieJar []
  , responseClose'    = ResponseClose (error "not needed")
  }

servantResponse :: L.ByteString -> [Header] -> Servant.Response
servantResponse body headers = Servant.Response
  { responseStatusCode  = status500
  , responseHeaders     = fromList headers
  , responseHttpVersion = http11
  , responseBody        = body
  }

serviceError :: ServiceError
serviceError = ServiceError'
  { _serviceAbbrev    = fromRight' (fromText "Oh balls")
  , _serviceStatus    = status500
  , _serviceHeaders   = headersToCensor
  , _serviceCode      = ErrorCode "Ooops"
  , _serviceMessage   = Nothing
  , _serviceRequestId = Nothing
  }

headersToCensor :: [Header]
headersToCensor = do
  let whitelistHeaders = (, "UNCENSORED") <$> Set.toList headerNameWhitelist
      otherHeaders     = [ ("X-Amz-Security-Token", "Super secret")
                         , ("Authorization", "I can pwn you with this") ]
  whitelistHeaders <> otherHeaders

expectedCensoredHeaders :: [Header]
expectedCensoredHeaders = do
  let whitelistHeaders = (, "UNCENSORED") <$> Set.toList headerNameWhitelist
      otherHeaders     = [ ("X-Amz-Security-Token", "**REDACTED**")
                         , ("Authorization", "**REDACTED**") ]
  whitelistHeaders <> otherHeaders
