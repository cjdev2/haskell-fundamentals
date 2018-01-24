{-# LANGUAGE OverloadedStrings #-}

module ParserCombinatorSpec where

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Applicative ((<|>), many, some)
import Data.Attoparsec.Text
import Data.Semigroup ((<>))
import Test.Hspec

data Devon
  = DevonNull
  | DevonString T.Text
  | DevonArray [Devon]
  | DevonMap (M.Map Devon Devon)
  deriving (Eq, Show)

elementP :: Parser Devon
elementP = DevonString <$> stringP
       <|> (DevonNull <$ string "()")
  where
    stringP :: Parser T.Text
    stringP = unquotedStringP <|> quotedStringP <?> "string"

    unquotedStringP :: Parser T.Text
    unquotedStringP = T.pack <$> some (satisfy (notInClass (whitespaceClass ++ structuralClass)))

    quotedStringP :: Parser T.Text
    quotedStringP = char '\'' *> (T.pack <$> many (satisfy (notInClass "'"))) <* char '\''

    whitespaceClass, structuralClass :: [Char]
    whitespaceClass = "\t\n\r "
    structuralClass = "'()[]{}"

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
        handleResult (Fail _ expected unexpected) = Left ("expected: " <> mconcat (map T.pack expected) <> ", failed with: " <> T.pack unexpected)
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
