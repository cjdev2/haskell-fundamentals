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
        buildNumFromList [1, 2 ,3] `shouldBe` 123
