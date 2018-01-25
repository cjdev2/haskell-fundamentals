module AlternativeSpec where

import Control.Applicative (Alternative(..))
import Control.Monad (guard)
import Test.Hspec

-- (<|>) is pronounced "or"

spec :: Spec
spec = do
  describe "empty" $ do
    it "empty on Maybe" $ do
      empty `shouldBe` (Nothing :: Maybe ())

    it "empty on []" $ do
      empty `shouldBe` ([] :: [()])

  describe "(<|>)" $ do
    it "(<|>) on Maybe" $ do
      -- Applicative/Monad instances for Maybe are short-circuiting, so (<|>) is short-circuiting
      (Nothing <|> Nothing) `shouldBe` (Nothing :: Maybe Integer)
      (Just 1 <|> Nothing) `shouldBe` Just 1
      (Nothing <|> Just 2) `shouldBe` Just 2
      (Just 1 <|> Just 2) `shouldBe` Just 1

    it "(<|>) on []" $ do
      -- Applicative/Monad instances for [] are cartesian product/ambiguous choice, so (<|>) appends
      ([] <|> []) `shouldBe` ([] :: [Integer])
      ([1, 2, 3] <|> []) `shouldBe` [1, 2, 3]
      ([] <|> [4, 5, 6]) `shouldBe` [4, 5, 6]
      ([1, 2, 3] <|> [4, 5, 6]) `shouldBe` [1, 2, 3, 4, 5, 6]

  describe "guard" $ do
    it "guard on Maybe" $ do
      guard False `shouldBe` Nothing
      guard True `shouldBe` Just ()

      let positiveProduct :: Integer -> Integer -> Maybe Integer
          positiveProduct x y = do
            let product = x * y
            guard (product > 0)
            pure product
      positiveProduct 2 3 `shouldBe` Just 6
      positiveProduct (-2) 3 `shouldBe` Nothing

    it "guard on List" $ do
      guard False `shouldBe` []
      guard True `shouldBe` [()]



