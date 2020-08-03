module TestUtils where

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

testGPX = GPX { segments = [testSegment], trackName = Just "test" }
