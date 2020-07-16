module Main where

import Visualization
import Types
import ParseGPX
import Data.Either (fromRight, fromLeft)

testDataFromFile :: IO GPX
testDataFromFile = do
  fdt <- readFile "./test_data/route_2020-07-12_1.51pm.gpx"
  return $ fromRight (GPX [] Nothing) (parseGPX fdt)

main :: IO ()
main = do
  testData <- testDataFromFile
  putStrLn $ writeData testData
  putStrLn $ drawMap testData 40 20
  
