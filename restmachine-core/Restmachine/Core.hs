{-# LANGUAGE OverloadedStrings #-}
module Restmachine.Core
  ( run
  , run'
  ) where

import Control.Monad ((<=<))

import Network.HTTP.Types.Status (status404)

import Restmachine.Core.Flow (runFlow)
import Restmachine.Core.Types

run :: Request -> Resource -> IO (Response, [Decision])
run = runFlow

run' :: Request -> Resource -> IO Response
run' req = return . fst <=< run req