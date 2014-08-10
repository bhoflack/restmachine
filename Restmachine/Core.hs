{-# LANGUAGE OverloadedStrings #-}
module Restmachine.Core
  ( run
  ) where

import Network.HTTP.Types.Status (status404)

import Restmachine.Core.Types

run :: Request a -> IO Response
run req = return response
  where
  response = Response status404 [] ""