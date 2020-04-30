

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HCube.All

main :: IO ()
main = hspec $ do
  describe "HCube" $ do
    describe "isCanonical" $ do
      context "when provided with valid cubes" $ do
        it "should return true on a valid 3 by 3" $ do
          (isCanonical (solved 3)) `shouldBe` True
        it "should return true on a valid 2 by 2" $ do
          (isCanonical (solved 2)) `shouldBe` True
      context "when provided with invalid cubes" $ do
        it "should return false on an invalid cube with wrong middle cubies" $ do
          (isCanonical (setCubie 31 White (setCubie 22 Red (solved 3)))) `shouldBe` False
        it "should return false on an invalid cube with wrong number of cubies" $ do
          (isCanonical (setCubie 22 Red (solved 3))) `shouldBe` False
        it "should return false on an invalid cube with two wrong edge cubies" $ do
          (isCanonical ((setCubie 19 Red (setCubie 28 White (solved 3))))) `shouldBe` False
        it "should return false on an invalid cube with wrong edge cubie" $ do
          (isCanonical ((setCubie 7 White (setCubie 19 Blue (solved 3))))) `shouldBe` False
        it "should return false on an invalid cube with two wrong corner cubies" $ do
          (isCanonical (setCubie 8 White (setCubie 27 Blue (setCubie 20 Red (((setCubie 6 Orange (setCubie 11 White (setCubie 18 Blue (solved 3)))))))))) `shouldBe` False
        it "should return false on an invalid cube with wrong corner cubie" $ do
          (isCanonical (setCubie 8 White (setCubie 27 Blue (setCubie 20 Red (solved 3))))) `shouldBe` False
  describe "solved" $ do
    it "returns only produces canonical cubes" $
      property $ prop_solved_isCanonical

prop_solved_isCanonical :: (Positive Int) -> Bool
prop_solved_isCanonical (Positive i) = (isCanonical . solved) i
