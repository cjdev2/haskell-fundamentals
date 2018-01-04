module DataSpec where
    
import Test.Hspec

data Point = Point Int Int deriving (Show, Eq)
data Rectangle = Rectangle Point Point deriving (Show, Eq)
data PointWithNamedFields = PointWithNamedFields { x::Int, y::Int } deriving (Show, Eq)
data Coordinate = Coordinate Int Int

describeCoordinate :: Coordinate -> [Char]
describeCoordinate (Coordinate 1 2) = "looks like baseline"
describeCoordinate (Coordinate 1 _) = "starts with 1"
describeCoordinate (Coordinate x 2) = "ends with 2, x is " ++ (show x)
describeCoordinate (Coordinate x y) = "another coordinate, x = " ++ (show x) ++ ", y = " ++ (show y)

spec :: Spec
spec = do
    it "data operations" $ do
        let a = Point 1 2
        let (Point b c) = a
        let d = show a
        let e = Point 1 2
        let f = Point 3 4
        let g = a == e
        let h = a == f
        b `shouldBe` 1
        c `shouldBe` 2
        d `shouldBe` "Point 1 2"
        g `shouldBe` True
        h `shouldBe` False

    it "data operations with named fields" $ do
        let a = PointWithNamedFields 1 2
        let (PointWithNamedFields b c) = a
        let d = show a
        let e = PointWithNamedFields 1 2
        let f = PointWithNamedFields 3 4
        let g = a == e
        let h = a == f
        let i = a { y = 5 }
        let j = x a
        let k = y a
        b `shouldBe` 1
        c `shouldBe` 2
        d `shouldBe` "PointWithNamedFields {x = 1, y = 2}"
        g `shouldBe` True
        h `shouldBe` False
        i `shouldBe` PointWithNamedFields 1 5
        j `shouldBe` 1
        k `shouldBe` 2
    
    it "nested data" $ do
        let a = Rectangle (Point 1 2) (Point 3 4)
        let (Rectangle (Point b c) d) = a
        let (Rectangle _ (Point e _)) = a
        a `shouldBe` Rectangle (Point 1 2) (Point 3 4)
        b `shouldBe` 1
        c `shouldBe` 2
        d `shouldBe` Point 3 4
        e `shouldBe` 3

    it "pattern match" $ do
        let baseline = Coordinate 1 2
        let sameAsBaseline = Coordinate 1 2
        let xDifferent = Coordinate 3 2
        let yDifferent = Coordinate 1 4
        let noMatch = Coordinate 3 3
        describeCoordinate baseline `shouldBe` "looks like baseline"
        describeCoordinate sameAsBaseline `shouldBe` "looks like baseline"
        describeCoordinate xDifferent `shouldBe` "ends with 2, x is 3"
        describeCoordinate yDifferent `shouldBe` "starts with 1"
        describeCoordinate noMatch `shouldBe` "another coordinate, x = 3, y = 3"
