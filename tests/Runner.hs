{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)

import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (Status)

import Restmachine.Core (run)
import Restmachine.Core.Types

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, (~=?))

import qualified Network.HTTP.Types.Status as H

defaultRequest = Request methodGet [] "hello world"

verifyResponse :: Resource -> Status -> Assertion
verifyResponse res stat = do
  resp <- run res defaultRequest
  assertEqual "" (Response stat [] "") resp

unavailable    = verifyResponse defaultResource H.status503
notImplemented = verifyResponse (defaultResource & serviceAvailable  .~ \_ -> return True) H.status501
longUri        = verifyResponse (defaultResource & (serviceAvailable .~ \_ -> return True)
                                                 & (knownMethod      .~ \_ -> return True)
                                                 & (uriTooLong       .~ \_ -> return True)) H.status414
methodNotAllowed = verifyResponse (defaultResource & (serviceAvailable .~ \_ -> return True)
                                                   & (knownMethod      .~ \_ -> return True)
                                                   & (methodAllowed    .~ \_ -> return False)) H.status405

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "run" [
      testCase "unavailable" unavailable
    , testCase "service not implemented" notImplemented
    , testCase "uri too long" longUri
    , testCase "method not allowed" methodNotAllowed
    ]
  ]