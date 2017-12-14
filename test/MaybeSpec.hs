module MaybeSpec where
    
import Test.Hspec

data User = User [Char] [Char] deriving (Show, Eq)

alice :: User
alice = User "Alice" "alicePassword"

searchForUserNamed :: [Char] -> Maybe User
searchForUserNamed name = 
    if name == "Alice"
        then Just alice
        else Nothing

searchAndDisplayResultAsString :: [Char] -> [Char]
searchAndDisplayResultAsString name = 
    let searchResult = searchForUserNamed name in
        case searchResult of
            (Just (User _ password)) -> "found user " ++ name ++ " with password " ++ password
            Nothing -> "did not find user " ++ name
  
spec :: Spec
spec = do
    it "find user by name" $ do
        searchForUserNamed "Alice" `shouldBe` Just alice

    it "search for user by name but don't find" $ do
        searchForUserNamed "Bob" `shouldBe` Nothing

    it "display search result when found" $ do
        let actual = searchAndDisplayResultAsString "Alice"
        let expected = "found user Alice with password alicePassword"
        actual `shouldBe` expected

    it "display search result when not found" $ do
        let actual = searchAndDisplayResultAsString "Bob"
        let expected = "did not find user Bob"
        actual `shouldBe` expected
                