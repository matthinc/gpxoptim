module ParserSpec ( spec ) where

import Test.Hspec
import ParseGPX
import Types


minimal = ("<gpx><trk><name>test</name><trkseg><trkpt lat=\"48.1382\" lon=\"11.6101\">\
            \<time>2020-06-17T15:36:09.109Z</time></trkpt></trkseg></trk></gpx>",
            GPX { segments = [ Segment [ Point { lat = 48.1382, lng = 11.6101 } ]], trackName = Just "test" })

empty = ("<gpx><trk><trkseg></trkseg></trk></gpx>",
          GPX { segments = [ Segment []], trackName = Nothing })

spec :: Spec
spec = do
  describe "parse gpx file" $ do
    it "parse minimal gpx file" $
      (parseGPX $ fst minimal) `shouldBe` (Right $ snd minimal)
    it "parse empty gpx file" $
      (parseGPX $ fst empty) `shouldBe` (Right $ snd empty)
      
