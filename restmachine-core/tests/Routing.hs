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

import Restmachine.Core (run')
import Restmachine.Core.Types
import Restmachine.Core.Routing (route)

import qualified Network.HTTP.Types.Status as H

defaultRequest = InitialRequest methodGet http11 [] "" []

resource1 = defaultResource & (serviceAvailable .~ static True)
                            & (knownMethod      .~ static True)
                            & (methodAllowed    .~ static True)
                            & (forbidden        .~ static False)
                            & (response         .~ (static $ Response H.status200 [] "resource1"))

instance Arbitrary Text where
  arbitrary = fromString <$> (arbitrary :: Gen String)

prop_identical uri = isJust res && "" == req' ^. displayPath
  where
  r = defaultResource
  req = defaultRequest & path .~ uri
  (res, req') = runState (route [(uri, r)]) req

prop_match_everything uri = isJust res && intercalate "/" uri == req' ^. displayPath
  where
  r = defaultResource
  req = defaultRequest & path .~ uri
  (res, req') = runState (route [(["*"], r)]) req

prop_subpath uri = isJust res
  where
  req = defaultRequest & path .~ ["a"] ++ uri
  (res, _) = runState (route [(["a", "*"], defaultResource)]) req

prop_nomatch uri = isNothing res
  where
  req = defaultRequest & path .~ uri
  (res, _) = runState (route [(["nomatch"], defaultResource)]) req

tests :: [Test]
tests = [
    testGroup "simple matchers" [
      testProperty "identical matcher" prop_identical
    , testProperty "match everything" prop_match_everything
    , testProperty "match the subpath" prop_subpath
    , testProperty "no match" prop_nomatch
    ]
  ]