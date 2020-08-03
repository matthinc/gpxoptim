module CommandsSpec ( spec ) where

import Test.Hspec
import Types
import TrimStart
import Commands

testSegment = Segment [
  Point { lat = 48.145427, lng = 11.613819},
  Point { lat = 48.1451335, lng = 11.6225522},
  Point { lat = 48.147907, lng = 11.632161},
  Point { lat = 48.157058, lng = 11.641963}
  ]

testGPX = GPX { segments = [testSegment], trackName = Just "test" }

spec :: Spec
spec = do
  describe "test commands" $ do
    it "test trim_start (10m)" $
      segments (trimStart testGPX 10) `shouldBe` [Segment { points = [Point { lat = 48.1451335, lng = 11.6225522}, Point { lat = 48.147907, lng = 11.632161}, Point { lat = 48.157058, lng = 11.641963}]}]
    it "test trim_start (600m)" $
      segments (trimStart testGPX 600) `shouldBe` [Segment { points = [Point { lat = 48.147907, lng = 11.632161}, Point { lat = 48.157058, lng = 11.641963}]}]
    it "test trim_start (1200m)" $
      segments (trimStart testGPX 1200) `shouldBe` [Segment { points = [Point { lat = 48.157058, lng = 11.641963}]}]
    it "test commands string 1" $
      runCommands "trim_start(600m);" testGPX `shouldBe` (trimStart testGPX 600)
    it "test commands string 2" $
      runCommands "trim_start  ( 600m  ) ;" testGPX `shouldBe` (trimStart testGPX 600)
    it "test commands string 3" $
      runCommands "trim_start(600m); trim_start(600m);" testGPX `shouldBe` (trimStart (trimStart testGPX 600) 600)
      
