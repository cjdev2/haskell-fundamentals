{-# LANGUAGE OverloadedStrings #-}

module MonoidSpec where

import Data.Monoid
import Data.Text (Text)
import Test.Hspec

-- Monoid extends Semigroup with the notion of an identity element, mempty

spec :: Spec
spec = do
  describe "mempty" $ do
    it "mempty for lists is empty list" $ do
      mempty `shouldBe` ([] :: [Integer])

    it "mempty on Text is the empty Text" $ do
      mempty `shouldBe` ("" :: Text)

    it "mempty on Sum is 0" $ do
      mempty `shouldBe` Sum 0

    it "mempty on Product is 1" $ do
      mempty `shouldBe` Product 1

    it "mempty on Maybe is Nothing" $ do
      mempty `shouldBe` (Nothing :: Maybe [Integer])

  describe "mconcat" $ do
    it "mconcat = foldr (<>) mempty" $ do
      foldr (<>) mempty [[1, 2], [3, 4], [5, 6]] `shouldBe` [1, 2, 3, 4, 5, 6]
      mconcat [[1, 2], [3, 4], [5, 6]] `shouldBe` [1, 2, 3, 4, 5, 6]
