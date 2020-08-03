module Visualization where

import Types
import GeoMaths
import qualified Data.Set as Set
import Data.Char (chr)

visualizeTrack :: GPX -> Int -> Int -> [String]
visualizeTrack track width height = map (\n -> row n) $ take height [height, height-1..]
  where grid = scaleToGrid track width height
        row num = concatMap (\n -> (cell n num)) $ take width [1,2..]
        cell x y = if Set.member (x, y) grid then "•" else " "

drawMap :: GPX -> Int -> Int -> String
drawMap track width height = frameTop ++ concatMap frameLine (visualizeTrack track width height) ++ frameBottom
  where frameTop = "╔" ++ (replicate width '═') ++ "╗\n"
        frameBottom = "╚" ++ (replicate width '═') ++ "╝"
        frameLine content = "║" ++ content ++ "║\n"

writeData :: GPX -> String
writeData track = "Name:     " ++ name ++ "\n" ++
                  "Length:   " ++ (formatDistance $ gpxLength track) ++ "\n" ++
                  "Segments: " ++ (show $ length $ segments track)
  where name = case trackName track of
          Nothing -> "(no name)"
          Just n -> n
        fillString str len = str ++ (replicate (len - length str) ' ')

formatDistance :: Double -> String
formatDistance dist
  | dist < 5000    = (show $ floor dist) ++ "m"
  | dist < 1000000 = (take 5 (show $ dist / 1000)) ++ "km"
  | otherwise      = (show (floor (dist / 1000))) ++ "km"
