module Main (main) where

import Data.IntervalMap as IntervalMap
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "IntervalMap" $ do
    -- All scopes nested
    it "let f = ... where g = ... where h = ..." $ do
      let f = IntervalMap.Interval 0 10 :: IntervalMap.Interval Int
          g = IntervalMap.Interval 5 10
          h = IntervalMap.Interval 7 10

          intervals =
            IntervalMap.insert f "f" $
              IntervalMap.insert g "g" $
                IntervalMap.insert h "h" $
                  empty

      shallowSearch intervals `shouldBe` [g]

-- Not yet defined
shallowSearch :: IntervalMap.IntervalMap k v -> [IntervalMap.Interval k]
shallowSearch = undefined
