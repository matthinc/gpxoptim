module ParserSpec ( spec ) where

import Test.Hspec
import ParseGPX
import Types

minimalGPX = "<gpx><trk><name>test</name><trkseg><trkpt lat=\"48.1382\" lng=\"11.6101\">\
             \<time>2020-06-17T15:36:09.109Z</time></trkpt></trkseg></trk></gpx>"

minimalGPXExpected = GPX { segments = [ Segment [ Point { lat = 48.1382, lng = 11.6101 } ]], trackName = Just "test" }

spec :: Spec
spec = do
  describe "parse gpx file" $ do
    it "parse minimal gpx file example" $
      (parseGPX minimalGPX) `shouldBe` (Right minimalGPXExpected)
      
