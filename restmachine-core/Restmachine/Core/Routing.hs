{-# LANGUAGE OverloadedStrings #-}
module Restmachine.Core.Routing
  ( route )
  where

import Control.Lens (view, (^.))
import Control.Monad.State (State (..), gets)
import Control.Monad.State.Lazy (modify)
import Data.List (find)
import Data.Text (Text, intercalate)
import Restmachine.Core.Types

-- | Route to the correct 'Resource' depending on the path of the 'Request'.
route :: [([Text], Resource)] -> State Request (Maybe Resource)
route hs = do
  p <- gets $ view path
  matchUrls hs p

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
  if m == p
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
