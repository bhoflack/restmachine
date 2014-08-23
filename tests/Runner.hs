{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((&), (.~), (^.))
import Data.ByteString (ByteString)

import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (Status)

import Restmachine.Core (run)
import Restmachine.Core.Types (Resource, Response (..), Request (..), defaultResource, static)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, (~=?))

import qualified Network.HTTP.Types.Status as H
import qualified Restmachine.Core.Types as T

defaultRequest = Request methodGet [] "hello world"

testResource = defaultResource & (T.serviceAvailable .~ static True)
                               & (T.knownMethod      .~ static True)
                               & (T.methodAllowed    .~ static True)
                               & (T.forbidden        .~ static False)
                               & (T.response         .~ (static $ Response H.status200 [] "<html><body>foo</body></html>"))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "statusCode" [
      testCase "service unavailable" serviceUnavailable
    , testCase "unknown method" unknownMethod
    , testCase "uri too long" uriTooLong
    , testCase "method not allowed" methodNotAllowed
    , testCase "bad request" badRequest
    , testCase "unauthorized" unauthorized
    , testCase "forbidden" forbidden
    , testCase "not implemented" notImplemented
    , testCase "unsupported media type" unsupportedMediaType
    , testCase "request too large" requestTooLarge
    , testCase "html response" responseHtml
    ]
  ]

serviceUnavailable = expectStatus H.status503
                                  (testResource & T.serviceAvailable .~ static False)

unknownMethod = expectStatus H.status501
                             (testResource & T.knownMethod .~ static False)

uriTooLong = expectStatus H.status414
                          (testResource & T.uriTooLong .~ static True)

methodNotAllowed = expectStatus H.status405
                                (testResource & T.methodAllowed .~ static False)

badRequest = expectStatus H.status400
                          (testResource & T.malformed .~ static True)

unauthorized = expectStatus H.status401
                            (testResource & T.authorized .~ static False)

forbidden = expectStatus H.status403
                         (testResource & T.forbidden .~ static True)

notImplemented = expectStatus H.status501
                              (testResource & T.unknownContentHeader .~ static True)

unsupportedMediaType = expectStatus H.status415
                                    (testResource & T.unknownContentType .~ static True)

requestTooLarge = expectStatus H.status413
                               (testResource & T.requestEntityTooLarge .~ static True)

responseHtml = do
  resp <- run testResource defaultRequest
  assertEqual "" "<html><body>foo</body></html>" (resp ^. T.responseBody)
  assertEqual "" H.status200 $ resp ^. T.responseStatus

expectStatus :: Status -> Resource -> Assertion
expectStatus stat res = do
  resp <- run res defaultRequest
  assertEqual "" stat (resp ^. T.responseStatus)