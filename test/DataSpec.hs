module DataSpec where
    
import Test.Hspec

data Point = Point Int Int deriving (Show, Eq)
data Rectangle = Rectangle Point Point deriving (Show, Eq)
data PointWithNamedFields = PointWithNamedFields { x::Int, y::Int } deriving (Show, Eq)
data Coordinate = Coordinate Int Int

spec :: Spec
spec = do
  it "data operations" $ do
    let a = Point 1 2
        (Point b c) = a
        d = Point 1 2
        e = Point 3 4
    b `shouldBe` 1
    c `shouldBe` 2
    show a `shouldBe` "Point 1 2"
    a == d `shouldBe` True
    a == e `shouldBe` False

  it "data operations with named fields" $ do
    let a = PointWithNamedFields 1 2
        (PointWithNamedFields b c) = a
        d = PointWithNamedFields 1 2
        e = PointWithNamedFields 3 4
    b `shouldBe` 1
    c `shouldBe` 2
    show a `shouldBe` "PointWithNamedFields {x = 1, y = 2}"
    a == d `shouldBe` True
    a == e `shouldBe` False
    a { y = 5 } `shouldBe` PointWithNamedFields 1 5
    x a `shouldBe` 1
    y a `shouldBe` 2
    
  it "nested data" $ do
    let a = Rectangle (Point 1 2) (Point 3 4)
        (Rectangle (Point b c) d) = a
        (Rectangle _ (Point e _)) = a
    a `shouldBe` Rectangle (Point 1 2) (Point 3 4)
    b `shouldBe` 1
    c `shouldBe` 2
    d `shouldBe` Point 3 4
    e `shouldBe` 3

  it "pattern match" $ do
    let describeCoordinate :: Coordinate -> [Char]
        describeCoordinate (Coordinate 1 2) = "looks like baseline"
        describeCoordinate (Coordinate 1 _) = "starts with 1"
        describeCoordinate (Coordinate x 2) = "ends with 2, x is " ++ (show x)
        describeCoordinate (Coordinate x y) = "another coordinate, x = " ++ (show x) ++ ", y = " ++ (show y)
        baseline = Coordinate 1 2
        sameAsBaseline = Coordinate 1 2
        xDifferent = Coordinate 3 2
        yDifferent = Coordinate 1 4
        noMatch = Coordinate 3 3
    describeCoordinate baseline `shouldBe` "looks like baseline"
    describeCoordinate sameAsBaseline `shouldBe` "looks like baseline"
    describeCoordinate xDifferent `shouldBe` "ends with 2, x is 3"
    describeCoordinate yDifferent `shouldBe` "starts with 1"
    describeCoordinate noMatch `shouldBe` "another coordinate, x = 3, y = 3"
