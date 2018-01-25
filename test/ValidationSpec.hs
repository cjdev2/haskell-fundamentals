{-# LANGUAGE OverloadedStrings #-}

module ValidationSpec where

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Either.Validation
import Data.Semigroup ((<>))
import Data.Text (Text)
import Test.Hspec
import Text.Read (readMaybe)

data Part = Part Text Shape Integer
  deriving (Show, Eq)

data Shape
  = Triangle
  | Circle
  | Square
  deriving (Show, Eq)

disallowBlank :: Text -> Either Text Text
disallowBlank "" = Left "must not be blank"
disallowBlank notBlank = Right notBlank

requireNumber :: Text -> Either Text Integer
requireNumber str = case readMaybe (T.unpack str) of
  Just num -> Right num
  Nothing -> Left $ "must be a number, was '" <> str <> "'"

requireAtLeast :: Integer -> Integer -> Either Text Integer
requireAtLeast limit x
  | x >= limit = Right x
  | otherwise = Left $ "must be at least " <> T.pack (show limit) <> ", was " <> T.pack (show x)

requireAtMost :: Integer -> Integer -> Either Text Integer
requireAtMost limit x
  | x <= limit = Right x
  | otherwise = Left $ "must be at most " <> T.pack (show limit) <> ", was " <> T.pack (show x)

requirePresent :: Maybe a -> Either Text a
requirePresent (Just x) = Right x
requirePresent Nothing = Left "must be present"

validPartName :: Text -> Either Text Text
validPartName name
  | T.isInfixOf " " name = Left $ "must not contain spaces, was '" <> name <> "'"
  | otherwise = Right name

validPartShape :: Text -> Either Text Shape
validPartShape "triangle" = Right Triangle
validPartShape "circle" = Right Circle
validPartShape "square" = Right Square
validPartShape notAShape = Left $ "must be 'triangle', 'circle', or 'square', was '" <> notAShape <> "'"

validPartQuality :: Text -> Either Text Integer
validPartQuality = disallowBlank >=> requireNumber >=> requireAtLeast 1 >=> requireAtMost 100

spec :: Spec
spec = do
  it "disallow blank" $ do
    disallowBlank "" `shouldBe` Left "must not be blank"
    disallowBlank "not blank" `shouldBe` Right "not blank"

  it "require number" $ do
    requireNumber "aaa" `shouldBe` Left "must be a number, was 'aaa'"
    requireNumber "123" `shouldBe` Right 123

  it "require at least" $ do
    let requireAtLeastOne = requireAtLeast 1
    requireAtLeastOne (-1) `shouldBe` Left "must be at least 1, was -1"
    requireAtLeastOne 0 `shouldBe` Left "must be at least 1, was 0"
    requireAtLeastOne 50 `shouldBe` Right 50

  it "require at most" $ do
    let requireAtMost100 = requireAtMost 100
    requireAtMost100 50 `shouldBe` Right 50
    requireAtMost100 100 `shouldBe` Right 100
    requireAtMost100 150 `shouldBe` Left "must be at most 100, was 150"

  it "compose fail fast validation" $ do
    let validNumber = disallowBlank >=> requireNumber >=> requireAtLeast 1 >=> requireAtMost 100
        sampleInputs = ["", "aaa", "-1", "234", "50"]
        actualResults = map validNumber sampleInputs
    actualResults `shouldBe`
      [ Left "must not be blank"
      , Left "must be a number, was 'aaa'"
      , Left "must be at least 1, was -1"
      , Left "must be at most 100, was 234"
      , Right 50
      ]

  it "compose aggregated validation" $ do
    let sampleValidData = Map.fromList [("name", "bit"), ("shape", "triangle"), ("quality", "79")]
        sampleInvalidData = Map.fromList [("name", "bit and a bob"), ("shape", "trapezoid"), ("quality", "wat")]
        toValidation = eitherToValidation . first (:[])
        partFromStringValues values = Part
          <$> toValidation (requirePresent (Map.lookup "name" values) >>= validPartName)
          <*> toValidation (requirePresent (Map.lookup "shape" values) >>= validPartShape)
          <*> toValidation (requirePresent (Map.lookup "quality" values) >>= validPartQuality)
    partFromStringValues sampleValidData `shouldBe` Success (Part "bit" Triangle 79)
    partFromStringValues sampleInvalidData `shouldBe` Failure
      [ "must not contain spaces, was 'bit and a bob'"
      , "must be 'triangle', 'circle', or 'square', was 'trapezoid'"
      , "must be a number, was 'wat'"
      ]
