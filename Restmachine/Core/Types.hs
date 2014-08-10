{-# LANGUAGE TemplateHaskell #-}
module Restmachine.Core.Types where

import Control.Lens (makeLenses)
import Data.ByteString (ByteString)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Header (ResponseHeaders, RequestHeaders)
import Network.HTTP.Types.Status (Status)

-- | The 'Request' made.
data Request a = Request 
  { _requestMethod       :: Method
  , _requestHeaders      :: RequestHeaders
  , _body                :: a
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