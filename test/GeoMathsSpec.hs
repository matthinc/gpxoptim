module GeoMathsSpec ( spec ) where

import TestUtils

import Test.Hspec
import GeoMaths
import Types
import Data.Either (fromRight, fromLeft)
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "test mathematical functions for distance calculations" $ do
    it "calculate distance between two streets" $
      (Point { lat = 48.145427, lng = 11.613819} `distanceTo` Point { lat = 48.1451335, lng = 11.6225522}) `shouldSatisfy` (\n -> n > 500 && n < 600)
    it "calculate length of segment" $
      (segmentLength testSegment)  `shouldSatisfy` (\n -> n > 2100 && n < 2500)
    it "calculate length of complex route" $ do
      testData <- testDataFromFile
      (gpxLength testData) `shouldSatisfy` (\n -> n > 70000 && n < 80000)
    it "calculate bounds" $
      (segmentBounds testSegment) `shouldBe` Bounds (48.1451335, 11.613819, 48.157058, 11.641963)
    it "calculate bounds of complex route" $ do
      testData <- testDataFromFile
      (gpxBounds testData) `shouldBe` Bounds (47.972416, 11.348251, 48.137755, 11.610477)
    it "scale to grid complex route" $ do
      testData <- testDataFromFile
      (scaleToGrid testData 5 5) `shouldBe` Set.fromList [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2),(3,1),(3,2),(3,3),(3,4),(4,2),(4,3),(4,4),(4,5),(5,4)]
      
