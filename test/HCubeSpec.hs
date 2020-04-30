

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HCube

main :: IO ()
main = hspec $ do
  describe "HCube.solved" $ do
    it "returns only produces canonical cubes" $
      property $ prop_solved_isCanonical

prop_solved_isCanonical :: (Positive Int) -> Bool
prop_solved_isCanonical (Positive i) = (isCanonical . solved) i
