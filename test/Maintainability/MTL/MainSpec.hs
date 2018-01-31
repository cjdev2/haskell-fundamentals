{-# LANGUAGE OverloadedStrings #-}

module Maintainability.MTL.MainSpec where

import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

import Maintainability.MTL.Main
import Maintainability.MTL.Stubs

spec :: Spec
spec = describe "main" $ do
  let startTime = posixSecondsToUTCTime 0.0
      endTime = posixSecondsToUTCTime 0.000123
      ((), logMessages) = runIdentity $ mainLogic
        & runArgumentsT ["sample.txt"]
        & runFileSystemT [("sample.txt", "Alyssa")]
        & runLoggerT
        & runPresetClockT [startTime, endTime]

  it "prints two log messages" $
    length logMessages `shouldBe` 2

  it "prints a greeting as the first message" $
    (logMessages !! 0) `shouldBe` "Hello, Alyssa!"

  it "prints the elapsed time in milliseconds as the second message" $
    (logMessages !! 1) `shouldBe` "123 microseconds"
