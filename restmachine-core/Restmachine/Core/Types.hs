{-# LANGUAGE TemplateHaskell #-}

-- | The types used in the application.
module Restmachine.Core.Types where

import Control.Lens (makeLenses)
import Control.Monad.State (StateT (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Header (ResponseHeaders, RequestHeaders)
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.URI (Query)
import Network.HTTP.Types.Version (HttpVersion)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Types.Status as H

-- | The 'Request' made.
data Request = 
    InitialRequest 
      { _requestMethod       :: Method
      , _httpVersion         :: HttpVersion
      , _requestHeaders      :: RequestHeaders
      , _body                :: BSL.ByteString
      , _path                :: [Text]
      }
  | RoutedRequest
      { _requestMethod       :: Method
      , _httpVersion         :: HttpVersion
      , _requestHeaders      :: RequestHeaders
      , _body                :: BSL.ByteString
      , _path                :: [Text]
      , _displayPath         :: Text
      , _pathInfo            :: [(Text, Text)]
      }
  deriving (Show, Eq)
makeLenses ''Request

-- | The 'Response' to a 'Request'.
data Response = Response
  { _responseStatus     :: Status
  , _responseHeaders    :: ResponseHeaders
  , _responseBody       :: BSL.ByteString
  }
  deriving (Show, Eq)
makeLenses ''Response

-- | The definition of a REST 'Resource'.
data Resource = Resource
  { _serviceAvailable      :: Request -> IO Bool
  , _knownMethod           :: Request -> IO Bool
  , _uriTooLong            :: Request -> IO Bool
  , _methodAllowed         :: Request -> IO Bool
  , _malformed             :: Request -> IO Bool
  , _authorized            :: Request -> IO Bool
  , _forbidden             :: Request -> IO Bool
  , _unknownContentHeader  :: Request -> IO Bool
  , _unknownContentType    :: Request -> IO Bool
  , _requestEntityTooLarge :: Request -> IO Bool
  , _response              :: Request -> IO Response
  }
makeLenses ''Resource

-- | An 'Application' contains the the handlers that route to the correct 'Resource'.
data Application = Application
  { _routes      :: [([Text], Resource)]
  , _fallback    :: Resource
  }
makeLenses ''Application

data DefaultContext = DefaultContext

data Decision = B13 | B12 | B11 | B10 | B9 | B8 | B7 | B6 | B5 | B4 | B3
  deriving (Show, Eq)

static :: a -> Request -> IO a
static v _ = return v

defaultResource :: Resource
defaultResource = Resource { _serviceAvailable =        static True
                           , _knownMethod =             static True
                           , _uriTooLong =              static False
                           , _methodAllowed =           static True
                           , _malformed =               static False
                           , _authorized =              static True
                           , _forbidden =               static False
                           , _unknownContentHeader =    static False
                           , _unknownContentType =      static False
                           , _requestEntityTooLarge =   static False
                           , _response =                static $ Response H.ok200 [] BSL.empty
                           }