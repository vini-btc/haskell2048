import Test.Hspec
import TwoThousandAndFortyEight

main :: IO()
main = hspec $ do
    describe "TwoThousandAndFortyEight" $ do
        it "it can squash similar numbers" $ do
            squashToTheRight [0, 0, 2, 2] `shouldBe` [0, 0,  0, 4]
            squashToTheRight [0, 8, 8, 0] `shouldBe` [0, 0, 16, 0]
            squashToTheRight [4, 4, 0, 0] `shouldBe` [0, 8,  0, 0]
            squashToTheRight [2, 0, 0, 0] `shouldBe` [2, 0,  0, 0]
            squashToTheRight [0, 0, 0, 0] `shouldBe` [0, 0,  0, 0]

        it "it can do right movement" $ do
            move Right' [0, 0, 2, 2] `shouldBe` [0, 0, 0,  4]
            move Right' [0, 8, 8, 0] `shouldBe` [0, 0, 0, 16]
            move Right' [4, 4, 0, 0] `shouldBe` [0, 0, 0,  8]
            move Right' [2, 0, 0, 0] `shouldBe` [0, 0, 0,  2]
            move Right' [0, 0, 0, 0] `shouldBe` [0, 0, 0,  0]

        it "it can do left movement" $ do
            move Left' [0, 0, 2, 2] `shouldBe` [4,  0, 0, 0]
            move Left' [0, 8, 8, 0] `shouldBe` [16, 0, 0, 0]
            move Left' [4, 4, 0, 0] `shouldBe` [8,  0, 0, 0]
            move Left' [2, 0, 0, 0] `shouldBe` [2,  0, 0, 0]
            move Left' [0, 0, 0, 0] `shouldBe` [0,  0, 0, 0]
