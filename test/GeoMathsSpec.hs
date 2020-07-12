module GeoMathsSpec ( spec ) where

import Test.Hspec
import GeoMaths
import Types

testSegment = Segment [
  Point { lat = 48.145427, lng = 11.613819},
  Point { lat = 48.1451335, lng = 11.6225522},
  Point { lat = 48.147907, lng = 11.632161},
  Point { lat = 48.157058, lng = 11.641963}
  ]

spec :: Spec
spec = do
  describe "test mathematical functions for distance calculations" $ do
    it "calculate distance between two streets" $
      (Point { lat = 48.145427, lng = 11.613819} `distanceTo` Point { lat = 48.1451335, lng = 11.6225522}) `shouldSatisfy` (\n -> n > 500 && n < 600)
    it "calculate length of segment" $
      (segmentLength testSegment)  `shouldSatisfy` (\n -> n > 2100 && n < 2500)
      
