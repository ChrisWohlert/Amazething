{-# LANGUAGE NegativeLiterals #-}


import Test.Hspec
import Test.QuickCheck
import MazeGenerator
import Linear.V2

main :: IO ()
main = hspec $ do
  describe "MazeGenerator.getWalls" $ do
    it "returns correct walls" $ do
      getWalls (V2 1 0) (LinkedNode (V2 0 0) [Blocked (V2 1 0), Blocked (V2 0 1), Blocked (V2 -1 0), Blocked (V2 0 -1)]) `shouldBe` 
        [ ((V2 0.5 0.5), (V2 -0.5 0.5))
        , ((V2 -0.5 0.5), (V2 -0.5 -0.5))
        , ((V2 -0.5 -0.5), (V2 0.5 -0.5))]