module ListComprehensionSpec where

import Test.Hspec

spec :: Spec
spec = do
  it "create ranges" $ do
    let timesTwo = \x -> x * 2
    [1..5] `shouldBe` [1,2,3,4,5]
    [x*2 | x <- [1..5]] `shouldBe` [2,4,6,8,10]
  it "generators, local bindings, boolean guards" $ do
    let result = [show tuple ++ " sum is " ++ show sum ++ ", product is " ++ show product
                  | i <- [1..5]
                  , j <- [i+1..5]
                  , k <- [j+1..5]
                  , let tuple = (i,j,k)
                        sum = i + j + k
                        product = i * j * k
                  , sum /= 10
                  , product /= 10] in
      result `shouldBe` ["(1,2,3) sum is 6, product is 6"
                        ,"(1,2,4) sum is 7, product is 8"
                        ,"(1,3,4) sum is 8, product is 12"
                        ,"(1,3,5) sum is 9, product is 15"
                        ,"(2,3,4) sum is 9, product is 24"
                        ,"(2,4,5) sum is 11, product is 40"
                        ,"(3,4,5) sum is 12, product is 60"]
