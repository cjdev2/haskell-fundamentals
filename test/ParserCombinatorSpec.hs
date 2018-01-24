{-# LANGUAGE OverloadedStrings #-}

module ParserCombinatorSpec where

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Applicative ((<|>), many, some)
import Data.Attoparsec.Text
import Data.Functor (void)
import Data.Semigroup ((<>))
import Test.Hspec

data Devon
  = DevonNull
  | DevonString T.Text
  | DevonArray [Devon]
  | DevonMap (M.Map Devon Devon)
  deriving (Eq, Ord, Show)

elementP :: Parser Devon
elementP = DevonString <$> stringP
       <|> (DevonNull <$ string "()" <?> "null")
       <|> DevonArray <$> devonArrayP
       <|> DevonMap <$> devonMapP
  where
    stringP :: Parser T.Text
    stringP = unquotedStringP <|> quotedStringP <?> "string"

    unquotedStringP :: Parser T.Text
    unquotedStringP = T.pack <$> some (satisfy (notInClass (whitespaceClass ++ structuralClass)))

    quotedStringP :: Parser T.Text
    quotedStringP = char '\'' *> (T.pack <$> many quotedStringCharP) <* char '\''

    quotedStringCharP :: Parser Char
    quotedStringCharP = satisfy (notInClass "'") <|> ('\'' <$ string "''")

    whitespaceClass, structuralClass :: [Char]
    whitespaceClass = "\t\n\r "
    structuralClass = "'()[]{}"

    whitespaceP :: Parser ()
    whitespaceP = void $ many (satisfy (inClass whitespaceClass))

    devonArrayP :: Parser [Devon]
    devonArrayP = char '[' *> whitespaceP *> many (elementP <* whitespaceP) <* char ']' <?> "array"

    devonMapP :: Parser (M.Map Devon Devon)
    devonMapP = char '{' *> whitespaceP *> (M.fromList <$> pairsP) <* char '}' <?> "map"

    pairsP :: Parser [(Devon, Devon)]
    pairsP = many (pairP <* whitespaceP)

    pairP :: Parser (Devon, Devon)
    pairP = (,) <$> (elementP <* whitespaceP) <*> elementP

parseDevon :: T.Text -> ([Devon], Maybe T.Text)
parseDevon txt = case parseStep txt of
    Left msg -> ([], Just msg)
    Right (val, "") -> ([val], Nothing)
    Right (val, unconsumedInput) ->
      let (results, err) = parseDevon unconsumedInput
      in (val : results, err)
  where
    parseStep :: T.Text -> Either T.Text (Devon, T.Text)
    parseStep unconsumedInput = handleResult (parse elementP unconsumedInput)
      where
        handleResult (Fail _ expected unexpected) = Left ("expected: " <> T.intercalate ", " (map T.pack expected) <> ", failed with: " <> T.pack unexpected)
        handleResult (Partial f) = handleResult (f "")
        handleResult (Done remainingInput result) = Right (result, remainingInput)


spec :: Spec
spec = do
  it "unquoted string" $ do
    parseDevon "Hello" `shouldBe` ([DevonString "Hello"], Nothing)

  it "quoted string" $ do
    parseDevon "'Hello, haskell!'" `shouldBe` ([DevonString "Hello, haskell!"], Nothing)

  it "null" $ do
    parseDevon "()" `shouldBe` ([DevonNull], Nothing)

  it "simple array" $ do
    parseDevon "[string 'quoted string' ()]" `shouldBe` ([DevonArray [DevonString "string", DevonString "quoted string", DevonNull]], Nothing)
    parseDevon "[ string'quoted string'() ]" `shouldBe` ([DevonArray [DevonString "string", DevonString "quoted string", DevonNull]], Nothing)

  it "string with quote in it" $ do
    parseDevon "'Sean''s favorite format'" `shouldBe` ([DevonString "Sean's favorite format"], Nothing)

  it "empty string" $ do
    parseDevon "''" `shouldBe` ([DevonString ""], Nothing)

  it "map" $ do
    parseDevon "{a 1 b 2}" `shouldBe` ([DevonMap (M.fromList [(DevonString "a", DevonString "1"), (DevonString "b", DevonString "2")])], Nothing)

  it "be paranoid about edge cases" $ do
    parseDevon "[{a 1}]"   `shouldBe` ([DevonArray [DevonMap (M.fromList [(DevonString "a", DevonString "1")])]], Nothing)
    parseDevon "[[a]]"     `shouldBe` ([DevonArray [DevonArray [DevonString "a"]]], Nothing)
    parseDevon "{[a b] 1}" `shouldBe` ([DevonMap (M.fromList [(DevonArray [DevonString "a", DevonString "b"], DevonString "1")])], Nothing)
