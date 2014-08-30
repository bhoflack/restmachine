{-# LANGUAGE OverloadedStrings #-}
module Restmachine.Core.Routing
  ( route )
  where

import Control.Lens ((^.))
import Data.List (find)
import Data.Text (Text)
import Restmachine.Core.Types

route :: [([Text], Resource)] -> Request -> Maybe Resource
route hs req = return . snd =<< find (\(m, _) -> matchUrl m path) hs
  where
  path = req ^. pathInfo

matchUrl :: [Text] -> [Text] -> Bool
matchUrl [] [] = True
matchUrl ["*"] _ = True
matchUrl (m: ms) (p: ps) 
  | m == "*"     = True
  | m == p       = matchUrl ms ps
  | otherwise    = False
matchUrl _ _ = False