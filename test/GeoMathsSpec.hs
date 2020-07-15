module GeoMathsSpec ( spec ) where

import Test.Hspec
import GeoMaths
import ParseGPX
import Types
import Data.Either (fromRight, fromLeft)

testSegment = Segment [
  Point { lat = 48.145427, lng = 11.613819},
  Point { lat = 48.1451335, lng = 11.6225522},
  Point { lat = 48.147907, lng = 11.632161},
  Point { lat = 48.157058, lng = 11.641963}
  ]

testDataFromFile :: IO GPX
testDataFromFile = do
  fdt <- readFile "./test_data/route_2020-07-12_1.51pm.gpx"
  return $ fromRight (GPX [] Nothing) (parseGPX fdt)
  
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
      
