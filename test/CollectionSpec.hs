module CollectionSpec where
    
import Test.Hspec

spec :: Spec
spec = do
    it "list operations" $ do
        let a = [1, 2, 3]
        let b = [4, 5, 6]

        -- concatenate lists
        a ++ b `shouldBe` [1, 2, 3, 4, 5, 6]

        -- add to beginning
        100 : a `shouldBe` [100, 1, 2, 3]

        -- add to end
        a ++ [100] `shouldBe` [1, 2, 3, 100]

    it "a string is a list of chars" $ do
        let a = "abc"
        let b = "def"

        -- concatenate lists
        a ++ b `shouldBe` "abcdef"

        -- add to beginning
        'z' : a `shouldBe` "zabc"

        -- add to end
        a ++ ['z'] `shouldBe` "abcz"
