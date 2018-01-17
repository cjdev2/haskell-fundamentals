module FunctorSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "fmap is a tool for function composition" $ do
    -- fmap :: Functor f => (a -> b) -> (f a -> f b)
    -- examples of functors are Maybe, List, Either
    -- these also happen to be monads
    -- all monads are functors
    -- not all functors are monads

    it "fmap lifts pure functions to work on values inside Maybe" $ do
      let intToString :: Integer -> String
          intToString = show

          liftedIntToString :: Maybe Integer -> Maybe String
          liftedIntToString = fmap intToString

      liftedIntToString (Just 42) `shouldBe` Just "42"
      liftedIntToString Nothing `shouldBe` Nothing

    it "fmap lifts pure functions to work on values inside Either with concrete types" $ do
      let intToString :: Integer -> String
          intToString = show

          liftedIntToString :: Either String Integer -> Either String String
          liftedIntToString = fmap intToString

      liftedIntToString (Right 42) `shouldBe` Right "42"
      liftedIntToString (Left "error") `shouldBe` Left "error"

    it "fmap lifts pure functions to work on values inside Either with polymorphic type" $ do
      let intToString :: Integer -> String
          intToString = show

          liftedIntToString :: Either a Integer -> Either a String
          liftedIntToString = fmap intToString

      liftedIntToString (Right 42) `shouldBe` (Right "42" :: Either String String)
      liftedIntToString (Left "error") `shouldBe` Left "error"

    it "fmap lifts pure functions to work on values inside lists" $ do
      let intToString :: Integer -> String
          intToString = show

          liftedIntToString :: [Integer] -> [String]
          liftedIntToString = fmap intToString

      liftedIntToString [42] `shouldBe` ["42"]
      liftedIntToString [1, 2, 3] `shouldBe` ["1", "2", "3"]
      liftedIntToString [] `shouldBe` []


    it "fmap lifts pure functions to work on any functor" $ do
      let intToString :: Integer -> String
          intToString = show

          liftedIntToString :: Functor f => f Integer -> f String
          liftedIntToString = fmap intToString

      liftedIntToString (Just 42) `shouldBe` Just "42"
      liftedIntToString Nothing `shouldBe` Nothing
      liftedIntToString (Right 42) `shouldBe` (Right "42" :: Either String String)
      liftedIntToString (Left "error") `shouldBe` Left "error"
      liftedIntToString [42] `shouldBe` ["42"]
      liftedIntToString [1, 2, 3] `shouldBe` ["1", "2", "3"]
      liftedIntToString [] `shouldBe` []
