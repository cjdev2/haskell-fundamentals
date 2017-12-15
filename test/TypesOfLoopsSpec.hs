module TypesOfLoopsSpec where
    
import Test.Hspec
import qualified Data.Map as Map


type AliasMap = Map.Map [Char] [[Char]]

data ColorAliasBuilder = ColorAliasBuilder { currentColor::[Char], colorAliases::AliasMap } deriving Show

setCurrentColor :: ColorAliasBuilder -> [Char] -> ColorAliasBuilder
setCurrentColor builder color = builder { currentColor = color }

addAlias :: ColorAliasBuilder -> [Char]  -> ColorAliasBuilder
addAlias builder alias =
  let ColorAliasBuilder currentColor colorAliases = builder
      oldColorAliasList =  Map.findWithDefault [] currentColor colorAliases 
      newColorAliasList =  oldColorAliasList ++ [alias] 
      newColorAliases = Map.insert alias newColorAliasList colorAliases in
  ColorAliasBuilder currentColor colorAliases

-- updateBuilderWithLine:: ColorAliasBuilder -> [Char] -> ColorAliasBuilder
-- updateBuilderWithLine builder (ColorPatten colorName) = setCurrentColor builder colorName
-- updateBuilderWithLine builder (AliasPatten colorAlias) = addAlias builder colorAlias

-- buildColorAliasMap::[[Char]] -> AliasMap
-- buildColorAliasMap = foldl 

spec :: Spec
spec = do
    it "fold with builder" $ do
        1 `shouldBe` 1
