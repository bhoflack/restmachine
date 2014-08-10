{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (status200, status404)

import Restmachine.Core (run)
import Restmachine.Core.Types (Request (..), Response (..))

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, (~=?))

defaultRequest = Request methodGet [] "hello world"

noResources :: Assertion
noResources = do
  resp <- run defaultRequest
  assertEqual "" (Response status404 [] "") resp

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "run" [
      testCase "no resources" noResources
    ]
  ]