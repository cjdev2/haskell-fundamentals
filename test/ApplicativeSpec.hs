module ApplicativeSpec where

import Test.Hspec
import Control.Monad.State (runState)
import Control.Applicative

spec :: Spec
spec = do
  describe "pure" $ do
    it "for Maybe, wraps a value in Just" $ do
      pure () `shouldBe` Just ()
      pure True `shouldBe` Just True
      pure "hello" `shouldBe` Just "hello"

    it "for Either, wraps a value in Right" $ do
      -- Left side can be anything, here we specify String with a type annotation to avoid ambiguity
      pure () `shouldBe` (Right () :: Either String ())
      pure True `shouldBe` (Right True :: Either String Bool)
      pure "hello" `shouldBe` (Right "hello" :: Either String String)

    it "for list, wraps a value in a single-element list" $ do
      pure () `shouldBe` [()]
      pure True `shouldBe` [True]
      pure "hello" `shouldBe` ["hello"]

    it "for State, ignores the current state and leaves it unchanged" $ do
      runState (pure ()) (42 :: Integer) `shouldBe` ((), 42)
      runState (pure True) (42 :: Integer) `shouldBe` (True, 42)
      runState (pure "hello") (42 :: Integer) `shouldBe` ("hello", 42)

    it "for I/O, creates a computation that does no side-effects" $ do
      -- evaluates its argument in the IO monad and checks its result
      let shouldReturnInIO :: (Eq a, Show a) => IO a -> a -> IO ()
          shouldReturnInIO action value = do
            result <- action
            result `shouldBe` value

      pure () `shouldReturnInIO` ()
      pure True `shouldReturnInIO` True
      pure "hello" `shouldReturnInIO` "hello"

  describe "(<*>)" $ do
    -- (<$>) is an infix alias for fmap
    -- (<*>) is pronounced "apply" (it has no prefix alias)

    it "applies a function wrapped in Just to a value wrapped in Just" $ do
      Just (+ 1) <*> Nothing `shouldBe` Nothing
      Just (+ 1) <*> (Just 2) `shouldBe` Just 3

    it "using case...of to perform function application through Maybe values" $ do
      let addTwoMaybeIntegers :: Maybe Integer -> Maybe Integer -> Maybe Integer
          addTwoMaybeIntegers (Just x) (Just y) = Just (x + y)
          addTwoMaybeIntegers _        _        = Nothing

      addTwoMaybeIntegers (Just 1) (Just 2) `shouldBe` Just 3
      addTwoMaybeIntegers Nothing (Just 2) `shouldBe` Nothing
      addTwoMaybeIntegers (Just 1) Nothing `shouldBe` Nothing
      addTwoMaybeIntegers Nothing Nothing `shouldBe` Nothing

    it "(<*>) works with (<$>) to plumb function application through Maybe values" $ do
      ((+) <$> Just 1 <*> Just 2) `shouldBe` Just 3
      ((+) <$> Nothing <*> Just 2) `shouldBe` Nothing
      ((+) <$> Just 1 <*> Nothing) `shouldBe` Nothing
      ((+) <$> Nothing <*> Nothing) `shouldBe` Nothing

    it "using case...of to perform function application through Either values" $ do
      let addTwoEitherIntegers :: Either String Integer -> Either String Integer -> Either String Integer
          addTwoEitherIntegers (Right x) (Right y) = Right (x + y)
          addTwoEitherIntegers (Left x)  _         = Left x
          addTwoEitherIntegers _         (Left x)  = Left x

      addTwoEitherIntegers (Right 1) (Right 2) `shouldBe` Right 3
      addTwoEitherIntegers (Left "err") (Right 2) `shouldBe` Left "err"
      addTwoEitherIntegers (Right 1) (Left "err") `shouldBe` Left "err"
      addTwoEitherIntegers (Left "err 1") (Left "err 2") `shouldBe` Left "err 1"

    it "(<*>) works with (<$>) to plumb function application through Either values" $ do
      ((+) <$> Right 1 <*> Right 2) `shouldBe` (Right 3 :: Either String Integer)
      ((+) <$> Left "err" <*> Right 2) `shouldBe` Left "err"
      ((+) <$> Right 1 <*> Left "err") `shouldBe` Left "err"
      ((+) <$> Left "err 1" <*> Left "err 2") `shouldBe` Left "err 1"
