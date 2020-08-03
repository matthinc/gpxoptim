module Main where

import Visualization
import Types
import ParseGPX
import Data.Either (fromRight, fromLeft)
import System.Environment
import System.Exit
import Commands
import Export

gpxFromFile :: String -> IO (Either String GPX)
gpxFromFile filename = do
  fdt <- readFile filename
  return $ parseGPX fdt

main :: IO ()
main = do
  args <- getArgs
  if length args /= 3
    then argsError
    else start args

start :: [String] -> IO ()
start args = do
  gpx <- gpxFromFile (args !! 0)
  case gpx of
    Left err -> parseError err
    Right g  -> processGPX g (args !! 1) (args !! 2)
  
processGPX :: GPX -> String -> String -> IO ()
processGPX gpx commands outfile = do
  putStrLn "========== Before =========="
  visualizeGPX gpx
  -- Run commands
  let resultGPX = runCommands commands gpx in do
    writeFile outfile $ gpxToXml resultGPX
    putStrLn "========== After =========="
    visualizeGPX resultGPX
    

visualizeGPX :: GPX -> IO ()
visualizeGPX gpx = do
    putStrLn $ writeData gpx
    putStrLn $ drawMap gpx 40 20
  
argsError :: IO ()
argsError = do
  putStrLn "Usage: FILENAME COMMAND OUTPUT_FILE"
  exitWith (ExitFailure 1)

parseError :: String -> IO ()
parseError err = do
  putStrLn $ "Parse error: " ++ err
  exitWith (ExitFailure 1)
