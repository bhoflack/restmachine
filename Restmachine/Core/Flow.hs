{-# LANGUAGE OverloadedStrings
           , RankNTypes #-}
module Restmachine.Core.Flow
  ( runFlow
  ) where

import Control.Lens (Lens', (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer (WriterT (..), tell)
import Restmachine.Core.Types

import qualified Network.HTTP.Types.Status as H

runFlow res req = runWriterT $ b13 res req

b13 = expect serviceAvailable B13 b12 H.serviceUnavailable503
b12 = expect knownMethod B12 b11 H.notImplemented501
b11 = unless uriTooLong B11 b10 H.requestURITooLong414
b10 = expect methodAllowed B10 b9 H.methodNotAllowed405
b9 = unless malformed B9 b8 H.badRequest400
b8 = expect authorized B8 b7 H.unauthorized401
b7 = unless forbidden B7 b6 H.forbidden403
b6 = unless unknownContentHeader B6 b5 H.notImplemented501
b5 = unless unknownContentType B5 b4 H.unsupportedMediaType415
b4 = unless requestEntityTooLarge B4 b3 H.requestEntityTooLarge413

b3 :: Resource -> Request -> WriterT [Decision] IO Response
b3 res req = do
  tell [B3]
  r <- liftIO $ (res ^. response) req
  return r

expect :: Lens' Resource (Request -> IO Bool)
       -> Decision
       -> (Resource -> Request -> WriterT [Decision] IO Response)
       -> H.Status
       -> Resource
       -> Request
       -> WriterT [Decision] IO Response
expect l d t s res req = do
  tell [d]
  r <- liftIO $ (res ^. l) req
  if r
    then t res req
    else return $ Response s [] ""

unless :: Lens' Resource (Request -> IO Bool)
        -> Decision
        -> (Resource -> Request -> WriterT [Decision] IO Response)
        -> H.Status
        -> Resource
        -> Request
        -> WriterT [Decision] IO Response
unless l d t s res req = do
  tell [d]
  r <- liftIO $ (res ^. l) req
  if r
    then return $ Response s [] ""
    else t res req