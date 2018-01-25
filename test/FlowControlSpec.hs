{-# LANGUAGE MultiWayIf #-}

module FlowControlSpec where

import Test.Hspec

spec :: Spec
spec = do
  it "case statement" $ do
    let x = 1
        name = case x of { 1 -> "one"; 2 -> "two"; 3 -> "three"; _ -> "something else" }
    name `shouldBe` "one"

  it "case statement with declarations in where clause" $ do
    let name = case x of { 1 -> "one"; 2 -> "two"; 3 -> "three"; _ -> "something else"; } where { x = 1 }
    name `shouldBe` "one"

  it "case statement using indentation to infer semicolons and braces" $ do
    -- the right hand side of the binding must be indented at least one space past the beginning of the identifier
    let name = case x of
          1 -> "one"
          2 -> "two"
          3 -> "three"
          _ -> "something else"
          where x = 1
    name `shouldBe` "one"

  it "if statement" $ do
    let three = if mod x 2 == 0 then "even" else "odd" where x = 3
        four = if mod x 2 == 0 then "even" else "odd" where x = 4
    three `shouldBe` "odd"
    four `shouldBe` "even"

  it "if statement with indentation" $ do
    -- the right hand side of the binding must be indented at least one space past the beginning of the identifier
    let three =
          if mod x 2 == 0
            then "even"
            else "odd"
          where x = 3
        four =
          if mod x 2 == 0
            then "even"
            else "odd"
          where x = 4
    three `shouldBe` "odd"
    four `shouldBe` "even"

  it "use function for flow control" $ do
    let integerPlusOneOrZeroCase x = case x of
          Just n -> n + 1
          Nothing -> 0
        integerPlusOneOrZeroFunction x = maybe 0 (\n -> n + 1) x

    integerPlusOneOrZeroCase (Just 42) `shouldBe` 43
    integerPlusOneOrZeroCase Nothing `shouldBe` 0

    integerPlusOneOrZeroFunction (Just 42) `shouldBe` 43
    integerPlusOneOrZeroFunction Nothing `shouldBe` 0

  it "guards on functions" $ do
    let sign x
          | x < 0 = -1
          | x > 0 = 1
          | otherwise = 0
    sign (-5) `shouldBe` -1
    sign 0 `shouldBe` 0
    sign 5 `shouldBe` 1

  it "guards on case...of" $ do
    let sign x = case x of
          n | x < 0 -> -1
            | x > 0 -> 1
            | otherwise -> 0
    sign (-5) `shouldBe` -1
    sign 0 `shouldBe` 0
    sign 5 `shouldBe` 1

  it "pattern-matching plus guards" $ do
    let signMaybe (Just x)
          | x < 0 = -1
          | x > 0 = 1
          | otherwise = 0
        signMaybe Nothing = 0
    signMaybe (Just (-5)) `shouldBe` -1
    signMaybe (Just 0) `shouldBe` 0
    signMaybe (Just 5) `shouldBe` 1
    signMaybe Nothing `shouldBe` 0

  it "guards using multi-way if" $ do
    let sign x = if | x < 0 -> -1
                    | x > 0 -> 1
                    | otherwise -> 0
    sign (-5) `shouldBe` -1
    sign 0 `shouldBe` 0
    sign 5 `shouldBe` 1