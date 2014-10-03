{-# LANGUAGE OverloadedStrings #-}
module Restmachine.Core
  ( run
  , run'
  ) where

import Control.Monad ((<=<))

import Network.HTTP.Types.Status (status404)

import Restmachine.Core.Flow (runFlow)
import Restmachine.Core.Types

-- | Run a 'Request' through a 'Resource'.  
-- Returns a 'Response' and all 'Decision's that have been taken for debugging.
run :: Request -> Resource -> IO (Response, [Decision])
run = runFlow

-- | Run a 'Request' through a 'Resource'.  Only return the 'Response'.
run' :: Request -> Resource -> IO Response
run' req = return . fst <=< run req