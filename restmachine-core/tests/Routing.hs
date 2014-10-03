{-# LANGUAGE OverloadedStrings #-}
module Routing where

import Control.Applicative ((<$>))
import Control.Lens ((&), (.~), (^.))
import Control.Monad.State (runState)
import Data.Maybe (isJust, isNothing)
import Data.String (fromString)
import Data.Text (Text, intercalate)

import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Version (http11)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO)

import Restmachine.Core (run')
import Restmachine.Core.Types
import Restmachine.Core.Routing (route)

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as H
import qualified Test.QuickCheck.Monadic as Q

defaultRequest = InitialRequest methodGet http11 [] "" []

resource1 = defaultResource & (serviceAvailable .~ static True)
                            & (knownMethod      .~ static True)
                            & (methodAllowed    .~ static True)
                            & (forbidden        .~ static False)
                            & (response         .~ (static $ Response H.status200 [] "resource1"))


resource2 = defaultResource & (serviceAvailable .~ static True)
                            & (knownMethod      .~ static True)
                            & (methodAllowed    .~ static True)
                            & (forbidden        .~ static False)
                            & (response         .~ (static $ Response H.status200 [] "resource2"))

notFound = defaultResource & (serviceAvailable .~ static True)
                            & (knownMethod      .~ static True)
                            & (methodAllowed    .~ static True)
                            & (forbidden        .~ static False)
                            & (response         .~ (static $ Response H.status404 [] "not found"))

instance Arbitrary Text where
  arbitrary = fromString <$> (arbitrary :: Gen String)

prop_identical uri = monadicIO $ do
    resp <- Q.run $ run' req' res
    Q.assert $ resp ^. responseBody == "resource1" && "" == req' ^. displayPath
  where
  req = defaultRequest & path .~ uri
  app = Application [(uri, resource1), (["other"], resource2)] notFound
  (res, req') = runState (route app) req

prop_match_everything uri = monadicIO $ do
    resp <- Q.run $ run' req' res
    Q.assert $ resp ^. responseBody == "resource1" && intercalate "/" uri == req' ^. displayPath
  where
  req = defaultRequest & path .~ uri
  app = Application [(["*"], resource1)] notFound
  (res, req') = runState (route app) req

prop_subpath uri = monadicIO $ do
    resp <- Q.run $ run' req' res
    Q.assert $ resp ^. responseBody == "resource1" && intercalate "/" uri == req' ^. displayPath
  where
  req = defaultRequest & path .~ ["a"] ++ uri
  app = Application [(["other"], resource2), (["a", "*"], resource1)] notFound
  (res, req') = runState (route app) req

prop_nomatch uri = monadicIO $ do
    resp <- Q.run $ run' req' res
    Q.assert $ resp ^. responseBody == "not found"
  where
  req = defaultRequest & path .~ uri
  app = Application [(["nomatch"], resource1)] notFound
  (res, req') = runState (route app) req

prop_variable_at_the_end = monadicIO $
                           Q.forAllM pathGenerator (\uri ->
  let req = defaultRequest & path .~ uri ++ ["world"]
      app = Application [(uri ++ [":hello"], resource1)] notFound
      (res, req') = runState (route app) req
  in do 
    resp <- Q.run $ run' req' res
    Q.assert $ resp ^. responseBody == "resource1" && [("hello", "world")] == req' ^. pathInfo)

prop_variable_in_the_middle = monadicIO $
                              Q.forAllM pathGenerator (\uriBefore ->
                              Q.forAllM pathGenerator (\uriAfter ->
  let req = defaultRequest & path .~ uriBefore ++ ["world"] ++ uriAfter
      app = Application [(uriBefore ++ [":hello"] ++ uriAfter, resource1)] notFound
      (res, req') = runState (route app) req
  in do
    resp <- Q.run $ run' req' res
    Q.assert $ resp ^. responseBody == "resource1" && [("hello", "world")] == req' ^. pathInfo))

prop_multiple_variables = monadicIO $
                          Q.forAllM pathGenerator (\uri ->
  let req = defaultRequest & path .~ uri ++ ["Brecht", "1"]
      app = Application [(uri ++ [":name", ":number"], resource1)] notFound
      (res, req') = runState (route app) req
  in do
    resp <- Q.run $ run' req' res
    Q.assert $ resp ^. responseBody == "resource1" && [("number", "1"), ("name", "Brecht")] == req' ^. pathInfo)
  
pathGenerator :: Gen [Text]
pathGenerator = suchThat paths (\p -> not $ any isVariable p)
  where paths = arbitrary :: Gen [Text]

isVariable :: Text -> Bool
isVariable = T.isPrefixOf ":"

tests :: [Test]
tests = [
    testGroup "simple matchers" [
      testProperty "identical matcher" prop_identical
    , testProperty "match everything" prop_match_everything
    , testProperty "match the subpath" prop_subpath
    , testProperty "no match" prop_nomatch
    ]
    , testGroup "variable matchers" [
      testProperty "variable at the end" prop_variable_at_the_end
    , testProperty "variable in the middle" prop_variable_in_the_middle
    , testProperty "multiple variables" prop_multiple_variables
    ]
  ]