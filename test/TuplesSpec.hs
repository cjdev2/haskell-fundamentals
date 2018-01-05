module TuplesSpec where

import Test.Hspec

spec :: Spec
spec = do
  it "construct tuple and pull out all values by position" $ do
    -- you can combine multiple values into a tuple
    let a = (123, "abc", 4.56)
    -- you can pull values out of a tuple with a pattern match
    -- where the tuple constructor puts multiple values together, and the pattern takes them apart
    -- this is signified by the same syntax as creating a tuple, only on the other side of the equals sign, like so
        (b, c, d) = a
    b `shouldBe` 123
    c `shouldBe` "abc"
    d `shouldBe` 4.56

  it "construct tuple and pull out only some values" $ do
    -- you can combine multiple values into a tuple
    let a = (123, "abc", 4.56)
    -- if you only care about specific values, you can use an underscore to indicate what you don't care about
        (_, b, _) = a
    b `shouldBe` "abc"

  it "get tupled version of function" $ do
    let foo x y = (y, x)
        tupledFoo = uncurry foo
    foo 1 "a" `shouldBe` ("a", 1)
    tupledFoo (1, "a") `shouldBe` ("a", 1)

  it "get curried version of function" $ do
    let foo (x, y) = (y, x)
        curriedFoo = curry foo
    foo (1, "a") `shouldBe` ("a", 1)
    curriedFoo 1 "a" `shouldBe` ("a", 1)
