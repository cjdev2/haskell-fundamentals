module TypesOfLoopsSpec where

import Test.Hspec
import qualified Data.Map as Map
import Data.List (foldl', stripPrefix)

sampleLines :: [String]
sampleLines =
  [ "red"
  , "    scarlet"
  , "    ruby"
  , "    flame"
  , "green"
  , "    jade"
  , "    forest"
  , "    mint"
  , "blue"
  , "    turquoise"
  , "    azure"
  , "    sapphire"
  ]

type AliasMap = Map.Map String [String]

emptyColor :: String
emptyColor = "<none>"

--------------------------------------------------------------------------------
-- Fold solution

data ColorAliasBuilder = ColorAliasBuilder
  { currentColor :: String
  , colorAliases :: AliasMap
  } deriving (Show, Eq)

emptyBuilder :: ColorAliasBuilder
emptyBuilder = ColorAliasBuilder emptyColor Map.empty

setCurrentColor :: ColorAliasBuilder -> String -> ColorAliasBuilder
setCurrentColor builder color = builder { currentColor = color }

addAlias :: ColorAliasBuilder -> String  -> ColorAliasBuilder
addAlias builder alias =
  let ColorAliasBuilder currentColor colorAliases = builder
      oldColorAliasList = Map.findWithDefault [] currentColor colorAliases
      newColorAliasList = oldColorAliasList ++ [alias]
      newColorAliases = Map.insert currentColor newColorAliasList colorAliases
  in ColorAliasBuilder currentColor newColorAliases

updateBuilderWithLine :: ColorAliasBuilder -> String -> ColorAliasBuilder
updateBuilderWithLine builder currentLine = case stripPrefix "    " currentLine of
  Just colorAlias -> addAlias builder colorAlias
  Nothing -> setCurrentColor builder currentLine

foldColorAliasMap :: [String] -> AliasMap
-- Left folding over lists is always strict, so we should use foldl' instead of
-- foldl to avoid space leaks. For more information see:
-- https://wiki.haskell.org/Foldr_Foldl_Foldl%27
foldColorAliasMap lines = colorAliases $ foldl' updateBuilderWithLine emptyBuilder lines

--------------------------------------------------------------------------------
-- Recursive solution

addRemainingLines :: AliasMap -> String -> [String] -> AliasMap
addRemainingLines colorAliases currentColor remainingLines =
  case remainingLines of
    currentLine : newRemainingLines ->
      case stripPrefix "    " currentLine of
        Just alias ->
          let oldColorAliasList = Map.findWithDefault [] currentColor colorAliases
              newColorAliasList = oldColorAliasList ++ [alias]
              newColorAliases = Map.insert currentColor newColorAliasList colorAliases
          in addRemainingLines newColorAliases currentColor newRemainingLines
        Nothing -> addRemainingLines colorAliases currentLine newRemainingLines
    _ -> colorAliases

recurseColorAliasMap :: [String] -> AliasMap
recurseColorAliasMap lines = addRemainingLines Map.empty emptyColor lines

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  it "empty builder" $ do
    currentColor emptyBuilder `shouldBe` "<none>"
    colorAliases emptyBuilder `shouldBe` Map.empty

  it "set current color" $ do
    let builder = setCurrentColor emptyBuilder "red"
    currentColor builder `shouldBe` "red"

  it "add alias" $ do
    let redBuilder = ColorAliasBuilder "red" Map.empty
        redScarletMap = Map.fromList [("red", ["scarlet"])]
        redScarletBuilder = ColorAliasBuilder "red" redScarletMap
    addAlias redBuilder "scarlet" `shouldBe` ColorAliasBuilder "red" (Map.fromList [("red", ["scarlet"])])

  it "update builder with line" $ do
    updateBuilderWithLine emptyBuilder "red"
      `shouldBe` ColorAliasBuilder "red" (Map.fromList [])
    updateBuilderWithLine (ColorAliasBuilder "red" (Map.fromList [])) "    scarlet"
      `shouldBe` ColorAliasBuilder "red" (Map.fromList [("red", ["scarlet"])])

  it "fold color alias map" $ do
    foldColorAliasMap sampleLines `shouldBe` Map.fromList
      [ ("red", ["scarlet", "ruby", "flame"])
      , ("green", ["jade", "forest", "mint"])
      , ("blue", ["turquoise", "azure", "sapphire"])
      ]

  it "recurse color alias map" $ do
    recurseColorAliasMap sampleLines `shouldBe` Map.fromList
      [ ("red", ["scarlet", "ruby", "flame"])
      , ("green", ["jade", "forest", "mint"])
      , ("blue", ["turquoise", "azure", "sapphire"])
      ]
