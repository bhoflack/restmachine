{-# LANGUAGE OverloadedStrings #-}
module Flow where

import Control.Lens ((&), (.~), (^.))
import Data.ByteString (ByteString)

import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Version (http11)

import Restmachine.Core (run)
import Restmachine.Core.Types (Decision (..), Resource, Response (..), Request (..), defaultResource, static)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, (~=?))

import qualified Network.HTTP.Types.Status as H
import qualified Restmachine.Core.Types as T

defaultRequest = InitialRequest methodGet http11 [] "hello world" ["hello"]

testResource = defaultResource & (T.serviceAvailable .~ static True)
                               & (T.knownMethod      .~ static True)
                               & (T.methodAllowed    .~ static True)
                               & (T.forbidden        .~ static False)
                               & (T.response         .~ (static $ Response H.status200 [] "<html><body>foo</body></html>"))

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

serviceUnavailable = do
  expectStatus H.status503 resource
  expectPath [B13] resource
  where
  resource = (testResource & T.serviceAvailable .~ static False)

unknownMethod = do
  expectStatus H.status501 resource
  expectPath [B13, B12] resource                             
  where
  resource = (testResource & T.knownMethod .~ static False)

uriTooLong = do
  expectStatus H.status414 resource
  expectPath [B13, B12, B11] resource
  where
  resource = (testResource & T.uriTooLong .~ static True)

methodNotAllowed = do
  expectStatus H.status405 resource
  expectPath [B13, B12, B11, B10] resource
  where
  resource = (testResource & T.methodAllowed .~ static False)

badRequest = do
  expectStatus H.status400 resource
  expectPath [B13, B12, B11, B10, B9] resource
  where
  resource = (testResource & T.malformed .~ static True)

unauthorized = do
  expectStatus H.status401 resource
  expectPath [B13, B12, B11, B10, B9, B8] resource
  where
  resource = (testResource & T.authorized .~ static False)

forbidden = do
  expectStatus H.status403 resource
  expectPath [B13, B12, B11, B10, B9, B8, B7] resource
  where
  resource = (testResource & T.forbidden .~ static True)

notImplemented = do
  expectStatus H.status501 resource
  expectPath [B13, B12, B11, B10, B9, B8, B7, B6] resource
  where
  resource = (testResource & T.unknownContentHeader .~ static True)

unsupportedMediaType = do
  expectStatus H.status415 resource
  expectPath [B13, B12, B11, B10, B9, B8, B7, B6, B5] resource
  where
  resource = (testResource & T.unknownContentType .~ static True)

requestTooLarge = do
  expectStatus H.status413 resource
  expectPath [B13, B12, B11, B10, B9, B8, B7, B6, B5, B4] resource
  where
  resource = (testResource & T.requestEntityTooLarge .~ static True)

responseHtml = do
  (resp, _) <- run defaultRequest testResource
  assertEqual "" "<html><body>foo</body></html>" (resp ^. T.responseBody)
  assertEqual "" H.status200 $ resp ^. T.responseStatus
  expectPath [B13, B12, B11, B10, B9, B8, B7, B6, B5, B4, B3] testResource

expectPath :: [Decision] -> Resource -> Assertion
expectPath p r = do
  (_, path) <- run defaultRequest r
  assertEqual "" p path

expectStatus :: Status -> Resource -> Assertion
expectStatus stat res = do
  (resp, _) <- run defaultRequest res
  assertEqual "" stat (resp ^. T.responseStatus)