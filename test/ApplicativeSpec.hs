module ApplicativeSpec where

import Test.Hspec
import Control.Monad.State (runState)

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


-- TODO: demonstrate (<*>)