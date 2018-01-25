module LazinessSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do
  it "partial functions throw exceptions if invariants are not met" $ do
    -- "head" is unidiomatic in Haskell because it can lead to runtime errors!
    head [1, 2, 3] `shouldBe` 1
    evaluate (head []) `shouldThrow` anyException

  it "'evaluate' evaluates its argument shallowly" $ do
    evaluate (error "bork" :: Maybe Integer) `shouldThrow` errorCall "bork"
    (evaluate (Just (error "bork" :: Integer)) *> pure ()) `shouldReturn` ()

  it "a computation that throws an exception only throws if evaluated" $ do
    const 1 2 `shouldBe` 1
    const 1 (head []) `shouldBe` 1
    evaluate (const (head []) 2) `shouldThrow` anyException

  it "seq is magic, and it introduces an evaluation dependency between two values" $ do
    (1 `seq` 2) `shouldBe` 2
    evaluate (head [] `seq` 2) `shouldThrow` anyException
    const 1 (head [] `seq` 2) `shouldBe` 1

  it "force makes a value evaluated deeply when it is evaluated at all" $ do
    (evaluate (force (Just (error "bork" :: Integer))) *> pure ()) `shouldThrow` errorCall "bork"
