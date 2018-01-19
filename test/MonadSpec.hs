module MonadSpec where

import Control.Monad (join)
import Control.Monad.State (State, get, put, modify, runState)
import Data.Foldable
import Test.Hspec

-- A Rational is a ratio of two Integer values
safeDivide :: Rational -> Rational -> Maybe Rational
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

-- split a list into its head and tail
uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

spec :: Spec
spec = do
  it "divide first element by the second element with nested case...of" $ do
    let numbers = [1, 3, 5]
        quotient = case uncons numbers of
          Just (a, as) -> case uncons as of
            Just (b, _) ->
              safeDivide a b
            Nothing -> Nothing
          Nothing -> Nothing
    quotient `shouldBe` Just (1/3)

  it "divide first element by the second element with >>=" $ do
    let numbers = [1, 3, 5]
        quotient =
          uncons numbers >>= \(a, as) ->
            uncons as >>= \(b, _) ->
              safeDivide a b
    quotient `shouldBe` Just (1/3)

  it "divide first element by the second element with do notation" $ do
    let numbers = [1, 3, 5]
        quotient = do
          (a, as) <- uncons numbers
          (b, _) <- uncons as
          safeDivide a b
    quotient `shouldBe` Just (1/3)

  -- pure lifts the tuple into the list context
  -- would be the same thing if you wrapped it in a single element list like this: [(x, y)]
  -- see also ApplicativeSpec
  it "the list monad produces a cartesian product with >>=" $ do
    let tuples =
          [1, 3, 5] >>= \x ->
            [2, 4] >>= \y ->
              pure (x, y)
    tuples `shouldBe` [(1, 2), (1, 4), (3, 2), (3, 4), (5, 2), (5, 4)]

  it "the list monad produces a cartesian product with do notation" $ do
    let tuples = do
          x <- [1, 3, 5]
          y <- [2, 4]
          pure (x, y)
    tuples `shouldBe` [(1, 2), (1, 4), (3, 2), (3, 4), (5, 2), (5, 4)]

  it "state models a computation with a single mutable cell of state" $ do
    let incrementState :: State Integer ()
        incrementState = do
          currentState <- get
          let newState = currentState + 1
          put newState

        addThreeToState :: State Integer ()
        addThreeToState = do
          incrementState
          incrementState
          incrementState

    runState addThreeToState 10 `shouldBe` ((), 13)
    runState addThreeToState 42 `shouldBe` ((), 45)

  it "join flattens two layers of Maybe" $ do
    join (Just (Just True)) `shouldBe` Just True
    join (Just (Nothing :: Maybe Bool)) `shouldBe` Nothing
    join (Nothing :: Maybe (Maybe Bool)) `shouldBe` Nothing

  it "join flattens two layers of List" $ do
    join [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]
    join [[[1,2], [3,4]], [[5,6], [7,8]]] `shouldBe` [[1,2], [3,4], [5,6], [7,8]]

  it "sequence combines a list of Just values from left to right, or returns Nothing" $ do
    sequence [Just 1, Just 2, Just 3] `shouldBe` Just [1, 2, 3]
    sequence [Nothing, Just 2, Just 3] `shouldBe` Nothing
    sequence [Just 1, Just 2, Nothing] `shouldBe` Nothing

  it "sequence combines a list of Right values from left to right, or returns the first Left" $ do
    sequence [Right 1, Right 2, Right 3] `shouldBe` (Right [1, 2, 3] :: Either String [Integer])
    sequence [Left "err", Right 2, Right 3] `shouldBe` Left "err"
    sequence [Right 1, Right 2, Left "err"] `shouldBe` Left "err"
    sequence [Left "err 1", Right 2, Left "err 2"] `shouldBe` Left "err 1"

  it "sequence runs a set of State side-effects from left to right" $ do
    let increment = modify (+ 1)
        double = modify (* 2)

    runState (sequence [increment, increment, double]) 3 `shouldBe` ([(), (), ()], 10)
    runState (sequence [double, increment, increment]) 3 `shouldBe` ([(), (), ()], 8)

    -- sequence_ ignores the result, only runs actions for their side-effects
    runState (sequence_ [increment, increment, double]) 3 `shouldBe` ((), 10)
    runState (sequence_ [double, increment, increment]) 3 `shouldBe` ((), 8)

  it "mapM applies a function that produces a Maybe to a list and collects the results like sequence" $ do
    mapM uncons [[1, 2], [3, 4, 5]] `shouldBe` Just [(1, [2]), (3, [4, 5])]
    mapM uncons [[], [3, 4, 5]] `shouldBe` Nothing

    -- mapM_ ignores the result, only runs actions for their side-effects
    mapM_ uncons [[1, 2], [3, 4, 5]] `shouldBe` Just ()
    mapM_ uncons [[], [3, 4, 5]] `shouldBe` Nothing

  it "traverse is the same as mapM" $ do
    traverse uncons [[1, 2], [3, 4, 5]] `shouldBe` Just [(1, [2]), (3, [4, 5])]
    traverse uncons [[], [3, 4, 5]] `shouldBe` Nothing

    -- traverse_ ignores the result, only runs actions for their side-effects
    traverse_ uncons [[1, 2], [3, 4, 5]] `shouldBe` Just ()
    traverse_ uncons [[], [3, 4, 5]] `shouldBe` Nothing
