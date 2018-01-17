module FunctionSpec where

import Test.Hspec

buildNum :: Int -> Int -> Int -> Int
buildNum hundreds tens ones = hundreds * 100 + tens * 10 + ones

buildNumFromTuple :: (Int, Int, Int) -> Int
buildNumFromTuple (hundreds, tens, ones) = hundreds * 100 + tens * 10 + ones

buildNumFromList :: [Int] -> Int
buildNumFromList list = buildNumFromReversedList $ reverse list

buildNumFromReversedList :: [Int] -> Int
buildNumFromReversedList [] = 0
buildNumFromReversedList (x: xs) = x + 10 * buildNumFromReversedList xs

spec :: Spec
spec = do
  it "functions are automatically curried" $ do
    buildNum 1 2 3 `shouldBe` 123

  it "getting values from a tuple" $ do
    buildNumFromTuple (1, 2, 3) `shouldBe` 123

  it "getting values from a list" $ do
    buildNumFromList [1, 2, 3] `shouldBe` 123

  it "different ways to define functions" $ do
    let myNumbers = [1,2]
        timesTwo x = x * 2
    map timesTwo myNumbers `shouldBe` [2,4]
    map (\x -> x * 2) myNumbers `shouldBe` [2,4]
    map (* 2) myNumbers `shouldBe` [2,4]

  it "compose functions" $ do
    let timesTwo x = x * 2
        plusTwo x = x + 2
        timesTwoThenPlusTwoA = \x -> plusTwo (timesTwo x)
        plusTwoThenTimesTwoA = \x -> timesTwo (plusTwo x)
        timesTwoThenPlusTwoB = \x -> plusTwo $ timesTwo x
        plusTwoThenTimesTwoB = \x -> timesTwo $ plusTwo x
        timesTwoThenPlusTwoC = plusTwo . timesTwo
        plusTwoThenTimesTwoC = timesTwo . plusTwo
    timesTwoThenPlusTwoA 3 `shouldBe` 8
    plusTwoThenTimesTwoA 3 `shouldBe` 10
    timesTwoThenPlusTwoB 3 `shouldBe` 8
    plusTwoThenTimesTwoB 3 `shouldBe` 10
    timesTwoThenPlusTwoC 3 `shouldBe` 8
    plusTwoThenTimesTwoC 3 `shouldBe` 10
