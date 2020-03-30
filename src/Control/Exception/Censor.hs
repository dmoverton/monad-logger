module Control.Exception.Censor
    ( censorAndDisplayException
    , censorException
    , censorHttpException
    , censorAwsError
    , censorServiceError
    , censorServantError
    , censorHeaders
    , headerNameWhitelist
    , censorBearerToken
    ) where

import           Control.Applicative ((<|>))
import           Control.Exception.Base (Exception(..), SomeException(..), displayException)
import           Control.Lens ((%~), (&), (.~))
import qualified Data.ByteString.Lazy as Lazy
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (IsString)
import           Data.Typeable (cast)
import           Network.AWS.Types (Error(..), ServiceError, serviceHeaders)
import           Network.HTTP.Client
                  (HttpException(..), HttpExceptionContent(..), Request, requestHeaders)
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Types.Header (Header, HeaderName, hAccept, hContentType, hHost)
import           Servant.Client (ServantError(..))
import qualified Servant.Client as Servant
import           Text.Regex.Base (RegexMaker, makeRegexOpts)
import           Text.Regex.Lens (matchedString, regex)
import           Text.Regex.TDFA
                  (CompOption, ExecOption, Regex, RegexLike, blankCompOpt, blankExecOpt,
                  caseSensitive)
import           Text.Regex.TDFA.Text ()

censorAndDisplayException :: Exception e => e -> String
censorAndDisplayException = displayException . censorException

-- | Censors sensitive information contained within the Exception so that it can be displayed
-- in logging or other similar uses.
-- At present, this function censors HttpExceptions and ServiceErrors, which both could contain
-- AWS security authentication tokens.
-- If the Exception is not one of these types, then we just return the original Exception.
censorException :: Exception e => e -> e
censorException e =
  fromMaybe e $
    (cast . censorSomeException =<< cast e)
    <|> (cast . censorHttpException =<< cast e)
    <|> (cast . censorAwsError =<< cast e)
    <|> (cast . censorServantError =<< cast e)
  where
    censorSomeException :: SomeException -> SomeException
    censorSomeException (SomeException e') = SomeException $ censorException e'

censorHttpException :: HttpException -> HttpException
censorHttpException = \case
  HttpExceptionRequest request content -> HttpExceptionRequest (censorRequest request)
                                                               (censorContent content)
  urlException@InvalidUrlException{}   -> urlException
  where
    censorRequest :: Request -> Request
    censorRequest request = request { requestHeaders = censorHeaders $ requestHeaders request }

    censorContent :: HttpExceptionContent -> HttpExceptionContent
    censorContent = \case
      StatusCodeException response body       -> StatusCodeException (censorResponse' response)
                                                                     (censorBearerToken body)
      TooManyRedirects responses              -> TooManyRedirects $ censorResponse <$> responses
      ConnectionFailure exception             -> ConnectionFailure $ censorException exception
      InternalException exception             -> InternalException $ censorException exception
      other@OverlongHeaders                   -> other
      other@ResponseTimeout                   -> other
      other@ConnectionTimeout                 -> other
      other@InvalidStatusLine{}               -> other
      other@InvalidHeader{}                   -> other
      other@InvalidRequestHeader{}            -> other
      other@ProxyConnectException{}           -> other
      other@NoResponseDataReceived            -> other
      other@TlsNotSupported                   -> other
      other@WrongRequestBodyStreamSize{}      -> other
      other@ResponseBodyTooShort{}            -> other
      other@InvalidChunkHeaders               -> other
      other@IncompleteHeaders                 -> other
      other@InvalidDestinationHost{}          -> other
      other@HttpZlibException{}               -> other
      other@InvalidProxyEnvironmentVariable{} -> other
      other@ConnectionClosed                  -> other
      other@InvalidProxySettings{}            -> other

    censorResponse :: Http.Response Lazy.ByteString -> Http.Response Lazy.ByteString
    censorResponse response =
      response
        { Http.responseHeaders = censorHeaders $ Http.responseHeaders response
        , Http.responseBody    = censorBearerToken $ Http.responseBody response }

    censorResponse' :: Http.Response () -> Http.Response ()
    censorResponse' response =
      response { Http.responseHeaders = censorHeaders $ Http.responseHeaders response }

censorAwsError :: Error -> Error
censorAwsError = \case
  TransportError httpException  -> TransportError $ censorHttpException httpException
  SerializeError serializeError -> SerializeError serializeError
  ServiceError   serviceError   -> ServiceError $ censorServiceError serviceError

censorServiceError :: ServiceError -> ServiceError
censorServiceError serviceError = serviceError & serviceHeaders %~ censorHeaders

censorServantError :: ServantError -> ServantError
censorServantError = \case
  FailureResponse response              -> FailureResponse $ censorResponse response
  DecodeFailure expectedType response   -> DecodeFailure expectedType $ censorResponse response
  UnsupportedContentType media response -> UnsupportedContentType media $ censorResponse response
  InvalidContentTypeHeader response     -> InvalidContentTypeHeader $ censorResponse response
  ConnectionError errorText             -> ConnectionError $ censorBearerToken errorText
  where
    censorResponse :: Servant.Response -> Servant.Response
    censorResponse response =
      response
        { Servant.responseHeaders = censorHeaders $ Servant.responseHeaders response
        , Servant.responseBody    = censorBearerToken $ Servant.responseBody response }

-- | For headers not in the header name whitelist, we remove their value.
-- Will preserve the order of the headers.
censorHeaders :: Functor t => t Header -> t Header
censorHeaders = fmap censorHeader

censorHeader :: Header -> Header
censorHeader header@(headerName, _)
  | headerName `Set.member` headerNameWhitelist = header
  | otherwise                                   = (headerName, "**REDACTED**")

-- | The header names that we do not censor.
headerNameWhitelist :: Set HeaderName
headerNameWhitelist = Set.fromList [hAccept, hContentType, hHost, "X-Amz-Request-Id"]

-- | Check for bearer tokens
-- "Bearer iAmAsecret" -> "Bearer **REDACTED**"
censorBearerToken :: forall a.
  (Monoid a, IsString a, RegexLike Regex a, RegexMaker Regex CompOption ExecOption a) => a -> a
censorBearerToken =
  let compileOpt = blankCompOpt { caseSensitive = False }
      bearerTokenRegex = makeRegexOpts compileOpt blankExecOpt ("Bearer ([a-z0-9_.-]+)" :: a)
  in regex bearerTokenRegex . matchedString .~ ("Bearer **REDACTED**" :: a)
