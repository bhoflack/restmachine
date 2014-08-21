{-# LANGUAGE OverloadedStrings
           , RankNTypes #-}
module Restmachine.Core.Flow
  ( runFlow
  ) where

import Control.Lens (Lens', (^.))
import Restmachine.Core.Types

import qualified Network.HTTP.Types.Status as H

runFlow = b13

b13 = ifPositive serviceAvailable b12 H.serviceUnavailable503
b12 = ifPositive knownMethod b11 H.notImplemented501
b11 = ifFalse uriTooLong b10 H.requestURITooLong414
b10 = ifPositive methodAllowed b9 H.methodNotAllowed405
b9 = ifFalse malformed b8 H.badRequest400
b8 = ifPositive authorized b7 H.unauthorized401
b7 = ifFalse forbidden b6 H.forbidden403
b6 = ifFalse unknownContentHeader b5 H.notImplemented501
b5 = ifFalse unknownContentType b4 H.unsupportedMediaType415
b4 = ifFalse requestEntityTooLarge b3 H.requestEntityTooLarge413

b3 :: Resource -> Request -> IO Response
b3 res req = do
  r <- (res ^. response) req
  return r

ifPositive :: Lens' Resource (Request -> IO Bool)
           -> (Resource -> Request -> IO Response)
           -> H.Status
           -> Resource
           -> Request
           -> IO Response
ifPositive l t s res req = do
  r <- (res ^. l) req
  if r
    then t res req
    else return $ Response s [] ""

ifFalse :: Lens' Resource (Request -> IO Bool) -> (Resource -> Request -> IO Response) -> H.Status -> Resource -> Request -> IO Response
ifFalse l t s res req = do
  r <- (res ^. l) req
  if r
    then return $ Response s [] ""
    else t res req