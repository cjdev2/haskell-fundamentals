{-# LANGUAGE OverloadedStrings #-}

module SemigroupSpec where

import Data.Text (Text)
import Data.Semigroup
import Test.Hspec

-- Semigroup defines (<>), an associative binary operator
-- (<>) is pronounced “append”

spec :: Spec
spec = do
  it "(<>) on lists appends lists" $ do
    ([1, 2] <> [3, 4]) `shouldBe` [1, 2, 3, 4]

  it "(<>) on Text appends Text" $ do
    ("hello, " <> "world!") `shouldBe` ("hello, world!" :: Text)

  it "(<>) on Sum adds numbers" $ do
    (Sum 3 <> Sum 4) `shouldBe` Sum 7

  it "(<>) on Product multiplies numbers" $ do
    (Product 3 <> Product 4) `shouldBe` Product 12

  it "(<>) on Maybe appends values inside Just" $ do
    (Just [1, 2] <> Just [3, 4]) `shouldBe` Just [1, 2, 3, 4]
    (Nothing <> Just [3, 4]) `shouldBe` Just [3, 4]
    (Just [1, 2] <> Nothing) `shouldBe` Just [1, 2]
    (Nothing <> Nothing) `shouldBe` (Nothing :: Maybe [Integer])
