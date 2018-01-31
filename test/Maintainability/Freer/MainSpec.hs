{-# LANGUAGE OverloadedStrings #-}

module Maintainability.Freer.MainSpec where

import Control.Monad.Freer (run)
import Data.Function ((&))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

import Maintainability.Freer.Main
import Maintainability.Freer.Stubs

spec :: Spec
spec = describe "main" $ do
  let startTime = posixSecondsToUTCTime 0.0
      endTime = posixSecondsToUTCTime 0.000123
      ((), logMessages) = run $ mainLogic
        & runArgumentsPure ["sample.txt"]
        & runFileSystemPure [("sample.txt", "Alyssa")]
        & runLogPure
        & runPresetClockPure [startTime, endTime]

  it "prints two log messages" $
    length logMessages `shouldBe` 2

  it "prints a greeting as the first message" $
    (logMessages !! 0) `shouldBe` "Hello, Alyssa!"

  it "prints the elapsed time in milliseconds as the second message" $
    (logMessages !! 1) `shouldBe` "123 microseconds"
