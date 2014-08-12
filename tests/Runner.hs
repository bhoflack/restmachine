{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)

import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (Status)

import Restmachine.Core (run)
import Restmachine.Core.Types (Resource, Response (..), Request (..), defaultResource)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, (~=?))

import qualified Network.HTTP.Types.Status as H
import qualified Restmachine.Core.Types as T

defaultRequest = Request methodGet [] "hello world"

verifyResponse :: Resource -> Status -> Assertion
verifyResponse res stat = do
  resp <- run res defaultRequest
  assertEqual "" (Response stat [] "") resp

unavailable    = verifyResponse defaultResource H.status503
notImplemented = verifyResponse (defaultResource & T.serviceAvailable  .~ \_ -> return True) H.status501
longUri        = verifyResponse (defaultResource & (T.serviceAvailable .~ \_ -> return True)
                                                 & (T.knownMethod      .~ \_ -> return True)
                                                 & (T.uriTooLong       .~ \_ -> return True)) H.status414
methodNotAllowed = verifyResponse (defaultResource & (T.serviceAvailable .~ \_ -> return True)
                                                   & (T.knownMethod      .~ \_ -> return True)
                                                   & (T.methodAllowed    .~ \_ -> return False)) H.status405
malformedRequest = verifyResponse (defaultResource & (T.serviceAvailable .~ \_ -> return True)
                                                   & (T.knownMethod      .~ \_ -> return True)
                                                   & (T.methodAllowed    .~ \_ -> return True)
                                                   & (T.malformed        .~ \_ -> return True)) H.status400
unauthorized = verifyResponse (defaultResource & (T.serviceAvailable .~ \_ -> return True)
                                               & (T.knownMethod      .~ \_ -> return True)
                                               & (T.methodAllowed    .~ \_ -> return True)
                                               & (T.authorized       .~ \_ -> return False)) H.status401
forbidden = verifyResponse (defaultResource & (T.serviceAvailable .~ \_ -> return True)
                                            & (T.knownMethod      .~ \_ -> return True)
                                            & (T.methodAllowed    .~ \_ -> return True)
                                            & (T.forbidden        .~ \_ -> return True)) H.status403

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "run" [
      testCase "unavailable" unavailable
    , testCase "service not implemented" notImplemented
    , testCase "uri too long" longUri
    , testCase "method not allowed" methodNotAllowed
    , testCase "malformed" malformedRequest
    , testCase "unauthorized" unauthorized
    , testCase "forbidden" forbidden
    ]
  ]