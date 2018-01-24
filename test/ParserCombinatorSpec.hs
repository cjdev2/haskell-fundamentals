{-# LANGUAGE OverloadedStrings #-}

module ParserCombinatorSpec where

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Applicative (some)
import Data.Attoparsec.Text
import Test.Hspec

data Devon
  = DevonNull
  | DevonString T.Text
  | DevonArray [Devon]
  | DevonMap (M.Map Devon Devon)
  deriving (Eq, Show)

elementP :: Parser Devon
elementP = DevonString <$> stringP
  where
    stringP :: Parser T.Text
    stringP = unquotedStringP

    unquotedStringP :: Parser T.Text
    unquotedStringP = T.pack <$> some (satisfy (notInClass (whitespaceClass ++ structuralClass)))

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
        handleResult (Fail _ _ msg) = Left (T.pack msg)
        handleResult (Partial f) = handleResult (f "")
        handleResult (Done remainingInput result) = Right (result, remainingInput)


spec :: Spec
spec = do
  it "work in progress, using vacuous test as placeholder" $ do
    parseDevon "Hello" `shouldBe` ([DevonString "Hello"], Nothing)
