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
  case searchResult of
    (Just (User _ password)) -> "found user " ++ name ++ " with password " ++ password
    Nothing -> "did not find user " ++ name
  where searchResult = searchForUserNamed name

data Baz = Baz (Maybe Integer)
data Bar = Bar (Maybe Baz)
data Foo = Foo (Maybe Bar)

withPatternMatch :: Maybe Foo -> Maybe Integer
withPatternMatch maybeFoo =
  case maybeFoo of
    Just (Foo maybeBar) -> case maybeBar of
      Just (Bar maybeBaz) -> case maybeBaz of
        Just (Baz maybeValue) -> case maybeValue of
          Just value -> Just (value * 2)
          Nothing -> Nothing
        Nothing -> Nothing
      Nothing -> Nothing
    Nothing -> Nothing

withDoBlock :: (Maybe Foo) -> (Maybe Integer)
withDoBlock maybeFoo = do
  Foo maybeBar <- maybeFoo
  Bar maybeBaz <- maybeBar
  Baz maybeValue <- maybeBaz
  value <- maybeValue
  pure (value * 2)

withBind :: (Maybe Foo) -> (Maybe Integer)
withBind maybeFoo =
  maybeFoo >>= \(Foo maybeBar) ->
  maybeBar >>= \(Bar maybeBaz) ->
  maybeBaz >>= \(Baz maybeValue) ->
  maybeValue >>= \value -> pure (value * 2)

spec :: Spec
spec = do
  it "find user by name" $ do
    searchForUserNamed "Alice" `shouldBe` Just alice

  it "search for user by name but don't find" $ do
    searchForUserNamed "Bob" `shouldBe` Nothing

  it "display search result when found" $ do
    let actual = searchAndDisplayResultAsString "Alice"
        expected = "found user Alice with password alicePassword"
    actual `shouldBe` expected

  it "display search result when not found" $ do
    let actual = searchAndDisplayResultAsString "Bob"
        expected = "did not find user Bob"
    actual `shouldBe` expected

  it "multiply the target value by 2 if it exists" $ do
    let testValues =
          [ Nothing
          , Just (Foo Nothing)
          , Just (Foo (Just (Bar Nothing)))
          , Just (Foo (Just (Bar (Just (Baz Nothing)))))
          , Just (Foo (Just (Bar (Just (Baz (Just 1234))))))
          ]
    map withPatternMatch testValues `shouldBe` [Nothing, Nothing, Nothing, Nothing, Just 2468]
    map withDoBlock testValues `shouldBe` [Nothing, Nothing, Nothing, Nothing, Just 2468]
    map withBind testValues `shouldBe` [Nothing, Nothing, Nothing, Nothing, Just 2468]
