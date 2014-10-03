{-# LANGUAGE OverloadedStrings #-}

-- | The code responsible for routing the 'Request' to the correct 'Resource'.
module Restmachine.Core.Routing
  ( route )
  where

import Control.Lens (view, (^.))
import Control.Monad.State (State (..), gets)
import Control.Monad.State.Lazy (modify)
import Data.List (find)
import Data.Text (Text, intercalate)
import Restmachine.Core.Types

import qualified Data.Text as T

-- | Route to the correct 'Resource' depending on the path of the 'Request'.
-- The first argument is an Application with a list of tuples containing the expression to match and
-- the 'Resource'.  When routing each of the handlers is iterated in order till we find
-- a handler that matches the path in the 'Request'.
-- When no match is found the fallback resource is used.
-- When matching the following rules are used:
--  - if a piece is identical to the path it matches
--  - if a piece is a * then it matches the rest of the path
--  - if a piece is a variable than it matches the other part and the pair is saved in the pathInfo
-- A 'RoutedRequest' is saved in the state containing the display path ( the path that matches
-- the * ) and the pathInfo ( the matched variables ).
route :: Application              -- ^ The application containing the routes
      -> State Request Resource   -- ^ Either the resource if matched or Nothing
route app = do
    p <- gets $ view path
    mres <- matchUrls hs p
    return $ maybe notfound id mres
  where
  hs = app ^. routes
  notfound = app ^. fallback

matchUrls :: [([Text], Resource)] -> [Text] -> State Request (Maybe Resource)
matchUrls [] p = return Nothing
matchUrls (h: hs) p = do
  mr <- matchUrl h p [] []
  case mr of
    Just r -> return $ Just r
    Nothing -> matchUrls hs p  

matchUrl :: ([Text], Resource) -> [Text] -> [Text] -> [(Text, Text)] -> State Request (Maybe Resource)
matchUrl ([], res) [] dp pi = do
  modify $ createRoutedRequest (intercalate "/" dp) pi
  return $ Just res

matchUrl (["*"], res) rp dp pi = do
  modify $ createRoutedRequest (intercalate "/" rp) pi
  return $ Just res

matchUrl ((m: ms), res) (p: ps) dp pi = do
  if isVariable m 
    then do
      matchUrl (ms, res) ps dp ((variableName m, p) : pi)
    else if m == p
      then matchUrl (ms, res) ps dp pi
      else return Nothing

matchUrl _ _ _ _ = return Nothing

createRoutedRequest :: Text -> [(Text, Text)] -> Request -> Request
createRoutedRequest dp pi r =
  RoutedRequest (r ^. requestMethod)
                (r ^. httpVersion)
                (r ^. requestHeaders)
                (r ^. body)
                (r ^. path)
                dp pi

isVariable :: Text -> Bool
isVariable = T.isPrefixOf ":"

variableName :: Text -> Text
variableName = T.drop 1
