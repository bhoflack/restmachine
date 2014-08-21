{-# LANGUAGE OverloadedStrings
           , RankNTypes #-}
module Restmachine.Core.Flow
  ( runFlow
  ) where

import Control.Lens (Lens', (^.))
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
b10 res req = do
  reqMethodAllowed <- isMethodAllowed req
  if reqMethodAllowed
    then b9 res req
    else return $ Response H.methodNotAllowed405 [] ""

  where
  isMethodAllowed = res ^. methodAllowed

b9 :: Resource -> Request -> IO Response
b9 res req = do
  reqMalformed <- isMalformed req
  if reqMalformed
    then return $ Response H.badRequest400 [] ""
    else b8 res req

  where
  isMalformed = res ^. malformed

b8 :: Resource -> Request -> IO Response
b8 res req = do
  reqAuthorized <- isAuthorized req
  if reqAuthorized
    then b7 res req
    else return $ Response H.unauthorized401 [] ""

  where
  isAuthorized = res ^. authorized

--b7 :: Resource -> Request -> IO Response
--b7 res req = do
-- reqForbidden <- isForbidden req
--  if reqForbidden
--    then return $ Response H.forbidden403 [] ""
--    else b6 res req

--  where
--  isForbidden = res ^. forbidden

b7 res req = junction forbidden (return $ Response H.forbidden403 [] "") (b6 res req) res req

b6 :: Resource -> Request -> IO Response
b6 res req = return $ Response H.notImplemented501 [] ""

junction :: Lens' Resource (Request -> IO Bool) -> (IO Response) -> (IO Response) -> Resource -> Request -> IO Response
junction l t f res req = do
  r <- (res ^. l) req
  if r
    then t
    else f