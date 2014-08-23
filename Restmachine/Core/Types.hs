{-# LANGUAGE TemplateHaskell #-}
module Restmachine.Core.Types where

import Control.Lens (makeLenses)
import Control.Monad.State (StateT (..))
import Data.ByteString (ByteString)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Header (ResponseHeaders, RequestHeaders)
import Network.HTTP.Types.Status (Status)

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types.Status as H

-- | The 'Request' made.
data Request = Request 
  { _requestMethod       :: Method
  , _requestHeaders      :: RequestHeaders
  , _body                :: ByteString
  } 
  deriving (Show, Eq)
makeLenses ''Request

-- | The 'Response' to a 'Request'.
data Response = Response
  { _responseStatus     :: Status
  , _responseHeaders    :: ResponseHeaders
  , _responseBody       :: ByteString
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

data DefaultContext = DefaultContext

static :: a -> Request -> IO a
static v _ = return v

defaultResource :: Resource
defaultResource = Resource { _serviceAvailable =        static False
                           , _knownMethod =             static False
                           , _uriTooLong =              static False
                           , _methodAllowed =           static False
                           , _malformed =               static False
                           , _authorized =              static True
                           , _forbidden =               static False
                           , _unknownContentHeader =    static False
                           , _unknownContentType =      static False
                           , _requestEntityTooLarge =   static False
                           , _response =                static $ Response H.ok200 [] BS.empty
                           }