{-# LANGUAGE OverloadedStrings #-}
module Restmachine.Core.Flow
  ( runFlow
  ) where

import Control.Lens ((^.))
import Restmachine.Core.Types

import qualified Network.HTTP.Types.Status as H

runFlow = b13

b13 :: Resource -> Request -> IO Response
b13 res req = do
  available <- isAvailable req
  if available
    then b12 res req
    else return $ Response H.serviceUnavailable503 [] ""

  where
  isAvailable = res ^. serviceAvailable

b12 :: Resource -> Request -> IO Response
b12 res req = do
  isKnown <- isKnownMethod req
  if isKnown
    then b11 res req
    else return $ Response H.notImplemented501 [] ""

  where
  isKnownMethod = res ^. knownMethod

b11 :: Resource -> Request -> IO Response
b11 res req = do
  uriTooLong <- isUriTooLong req
  if uriTooLong
    then return $ Response H.requestURITooLong414 [] ""
    else b10 res req

  where
  isUriTooLong = res ^. uriTooLong

b10 :: Resource -> Request -> IO Response
b10 res req = return $ Response H.methodNotAllowed405 [] ""
