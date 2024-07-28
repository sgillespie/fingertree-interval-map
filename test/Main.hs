module Main (main) where

import qualified Data.IntervalMap as IntervalMap
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "IntervalMap" $ do
    -- All scopes nested
    it "let f = ... where g = ... where h = ..." $ do
      let f = IntervalMap.Interval 0 10 :: IntervalMap.Interval Int
          g = IntervalMap.Interval 5 10
          h = IntervalMap.Interval 7 10
          intervals = IntervalMap.fromList [(f, "f"), (g, "g"), (h, "h")]

      shallowSearch intervals `shouldBe` [g]

-- Not yet defined
shallowSearch :: IntervalMap.IntervalMap k v -> [IntervalMap.Interval k]
shallowSearch = undefined
