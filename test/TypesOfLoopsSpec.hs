module TypesOfLoopsSpec where
    
import Test.Hspec
import qualified Data.Map as Map

sampleLines :: [[Char]]
sampleLines = [
    "red",
    "    scarlet",
    "    ruby",
    "    flame",
    "green",
    "    jade",
    "    forest",
    "    mint",
    "blue",
    "    turquoise",
    "    azure",
    "    sapphire"
    ]

type AliasMap = Map.Map [Char] [[Char]]

data ColorAliasBuilder = ColorAliasBuilder { currentColor::[Char], colorAliases::AliasMap } deriving (Show, Eq)

emptyColor :: [Char]
emptyColor = "<none>"

emptyBuilder :: ColorAliasBuilder
emptyBuilder = ColorAliasBuilder emptyColor Map.empty

setCurrentColor :: ColorAliasBuilder -> [Char] -> ColorAliasBuilder
setCurrentColor builder color = builder { currentColor = color }

addAlias :: ColorAliasBuilder -> [Char]  -> ColorAliasBuilder
addAlias builder alias =
  let ColorAliasBuilder currentColor colorAliases = builder
      oldColorAliasList = Map.findWithDefault [] currentColor colorAliases 
      newColorAliasList = oldColorAliasList ++ [alias] 
      newColorAliases = Map.insert currentColor newColorAliasList colorAliases in
  ColorAliasBuilder currentColor newColorAliases

updateBuilderWithLine :: ColorAliasBuilder -> [Char] -> ColorAliasBuilder
updateBuilderWithLine builder (' ' : ' ' : ' ' : ' ' : colorAlias) = addAlias builder colorAlias
updateBuilderWithLine builder colorName = setCurrentColor builder colorName

foldColorAliasMap::[[Char]] -> AliasMap
foldColorAliasMap lines = colorAliases $ foldl updateBuilderWithLine emptyBuilder lines

addRemainingLines::AliasMap -> [Char] -> [[Char]] -> AliasMap
addRemainingLines colorAliases currentColor remainingLines = 
    case remainingLines of 
        currentLine : newRemainingLines ->
            case currentLine of
                (' ' : ' ' : ' ' : ' ' : alias) ->
                    let oldColorAliasList = Map.findWithDefault [] currentColor colorAliases 
                        newColorAliasList = oldColorAliasList ++ [alias]
                        newColorAliases = Map.insert currentColor newColorAliasList colorAliases in
                    addRemainingLines newColorAliases currentColor newRemainingLines
                newColor -> addRemainingLines colorAliases newColor newRemainingLines
        _ -> colorAliases

recurseColorAliasMap::[[Char]] -> AliasMap
recurseColorAliasMap lines = addRemainingLines Map.empty emptyColor lines

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
        let redScarletMap = Map.fromList [("red", ["scarlet"])]
        let redScarletBuilder = ColorAliasBuilder "red" redScarletMap
        addAlias redBuilder "scarlet" `shouldBe` ColorAliasBuilder "red" (Map.fromList [("red", ["scarlet"])])
    
    it "update builder with line" $ do
        (updateBuilderWithLine emptyBuilder "red") `shouldBe` ColorAliasBuilder "red" (Map.fromList [])
        (updateBuilderWithLine (ColorAliasBuilder "red" (Map.fromList [])) "    scarlet") `shouldBe` ColorAliasBuilder "red" (Map.fromList [("red", ["scarlet"])])
    
    it "fold color alias map" $ do
        (foldColorAliasMap sampleLines) `shouldBe` Map.fromList[
            ("red", ["scarlet", "ruby", "flame"]),
            ("green", ["jade", "forest", "mint"]),
            ("blue", ["turquoise", "azure", "sapphire"])]

    it "recurse color alias map" $ do
        (recurseColorAliasMap sampleLines) `shouldBe` Map.fromList[
            ("red", ["scarlet", "ruby", "flame"]),
            ("green", ["jade", "forest", "mint"]),
            ("blue", ["turquoise", "azure", "sapphire"])]
