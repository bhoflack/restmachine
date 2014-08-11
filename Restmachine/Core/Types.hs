{-# LANGUAGE TemplateHaskell #-}
module Restmachine.Core.Types where

import Control.Lens (makeLenses)
import Control.Monad.State (StateT (..))
import Data.ByteString (ByteString)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Header (ResponseHeaders, RequestHeaders)
import Network.HTTP.Types.Status (Status)

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
  { _serviceAvailable   :: Request -> IO Bool
  , _knownMethod        :: Request -> IO Bool
  , _uriTooLong         :: Request -> IO Bool
  , _methodAllowed      :: Request -> IO Bool
  }
makeLenses ''Resource

data DefaultContext = DefaultContext

defaultResource :: Resource
defaultResource = Resource { _serviceAvailable = \_ -> return False
                           , _knownMethod =      \_ -> return False
                           , _uriTooLong =       \_ -> return False
                           , _methodAllowed =    \_ -> return False
                           }