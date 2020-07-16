module Visualization where

import Types
import GeoMaths
import qualified Data.Set as Set
import Data.Char (chr)

visualizeTrack :: GPX -> Int -> Int -> [String]
visualizeTrack track width height = map (\n -> row n) $ take height [height, height-1..]
  where grid = scaleToGrid track width height
        row num = concatMap (\n -> (cell n num)) $ take width [1,2..]
        cell x y = if Set.member (x, y) grid then "." else " "

drawMap :: GPX -> Int -> Int -> String
drawMap track width height = frameTop ++ concatMap frameLine (visualizeTrack track width height) ++ frameBottom
  where frameTop = "╔" ++ (replicate width '═') ++ "╗\n"
        frameBottom = "╚" ++ (replicate width '═') ++ "╝"
        frameLine content = "║" ++ content ++ "║\n"

writeData :: GPX -> String
writeData track = "======== " ++ name ++ " ========\n" ++
                  "Length:        " ++ (show $ floor $ gpxLength track) ++ "m\n" ++
                  "GPX segments : " ++ (show $ length $ segments track) ++ "\n"
  where name = case trackName track of
          Nothing -> "(no name)"
          Just n -> n

