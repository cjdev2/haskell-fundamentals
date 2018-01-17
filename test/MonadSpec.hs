module MonadSpec where

import Test.Hspec

spec :: Spec
spec = do
  it "work in progress, using vacuous test as placeholder" $ do
    True `shouldBe` True

-- todo: implement examples of the following
-- >>= vs. do
-- io, maybe, list, state
-- lift vs. ap
-- return, fail, guard, mzero
-- fmap, join
-- mapm, sequence
