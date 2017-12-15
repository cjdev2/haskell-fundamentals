module FlowControlSpec where
    
import Test.Hspec

spec :: Spec
spec = do
    it "case statement" $ do
        let x = 1
        let name = case x of { 1 -> "one"; 2 -> "two"; 3 -> "three"; other -> "something else" }
        name `shouldBe` "one"

    it "case statement with declarations in where clause" $ do
        let name = case x of { 1 -> "one"; 2 -> "two"; 3 -> "three"; other -> "something else"; } where { x = 1 }
        name `shouldBe` "one"

    it "case statement using indentation to infer semicolons and braces" $ do
        let name = case x of
                1 -> "one"
                2 -> "two" 
                3 -> "three" 
                other -> "something else"
                where x = 1
        name `shouldBe` "one"

    it "if statement" $ do
        let three = if mod x 2 == 0 then "even" else "odd" where x = 3
        let four = if mod x 2 == 0 then "even" else "odd" where x = 4
        three `shouldBe` "odd"
        four `shouldBe` "even"

    it "if statement with indentation" $ do
        let three = 
                if mod x 2 == 0 
                then "even" 
                else "odd" 
                where x = 3
        let four = 
                if mod x 2 == 0 
                then "even" 
                else "odd" 
                where x = 4
        three `shouldBe` "odd"
        four `shouldBe` "even"