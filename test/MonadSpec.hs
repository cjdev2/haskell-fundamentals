module MonadSpec where

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
  it "work in progress, using vacuous test as placeholder" $ do
    True `shouldBe` True

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

-- todo: implement examples of the following
-- io, list, state
-- lift vs. ap
-- return, fail, guard, mzero
-- fmap, join
-- mapm, sequence
