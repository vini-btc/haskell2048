module TwoThousandAndFortyEightSpec where

import Test.Hspec
import Test.QuickCheck
import TwoThousandAndFortyEight

main :: IO()
main = hspec $ do
    describe "TwoThousandAndFortyEight.moveRight" $ do
        it "moves non-zero number to right" $ do
            moveRight [2, 0, 0, 2] `shouldBe` [0, 0, 2, 2]
            moveRight [0, 2, 2, 0] `shouldBe` [0, 0, 2, 2]
            moveRight [0, 4, 2, 0] `shouldBe` [0, 0, 4, 2]
            moveRight [2, 0, 0, 0] `shouldBe` [0, 0, 0, 2]
            moveRight [2, 2, 0, 0] `shouldBe` [0, 0, 2, 2]

