{-# LANGUAGE OverloadedStrings #-}
module Restmachine.Core
  ( run
  ) where

import Network.HTTP.Types.Status (status404)

import Restmachine.Core.Flow (runFlow)
import Restmachine.Core.Types

run :: Resource -> Request -> IO (Response, [Decision])
run = runFlow