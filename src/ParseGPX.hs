module ParseGPX ( parseGPX ) where

import Xeno.DOM ( parse, Node, name, children, contents, Content (Text), attributes )
import Data.ByteString.Char8 ( pack, unpack )
import Data.List (find)
import Data.Either (rights)
import Data.Maybe (maybe)

import Types

-- | Parse GPX string to GPX-Object
parseGPX :: String -> Either String GPX
parseGPX gpx = case parse (pack gpx) of
  Left exception -> Left $ show exception
  Right node     -> processRootNode node

processRootNode :: Node -> Either String GPX
processRootNode node = case nodeName node of
  "gpx" -> processGPXNode node
  _     -> Left "Root-node should be of type 'gpx'"
  
processGPXNode :: Node -> Either String GPX
processGPXNode node = case track of
  Just t  -> processTrackNode t
  Nothing -> Left "'gpx' node does not contain 'trk' node"
  where track = find (\n -> (nodeName n) == "trk") (children node)

processTrackNode :: Node -> Either String GPX
processTrackNode node = Right $ GPX { trackName = trkName, segments = trkSegments }
  where trkName = case find (\n -> (nodeName n) == "name") (children node) of
          Nothing -> Nothing
          Just n  -> Just $ nodeContentString n
        trkSegments = rights $ map (processSegmentNode) filterSegments
        filterSegments = filter (\n -> (nodeName n) == "trkseg") (children node)

processSegmentNode :: Node -> Either String Segment
processSegmentNode node = Right $ Segment points
  where filterPoints = filter (\n -> (nodeName n) == "trkpt") (children node)
        points = rights $ map (processPointNode) filterPoints

processPointNode :: Node -> Either String Point
processPointNode node = case nodeName node of
  "trkpt" -> Right $ Point { lat = (nodeAttrDouble "lat"), lng = (nodeAttrDouble "lon") }
  _     -> Left "Node type should be 'trkpt'"
  where nodeAttrDouble attr = read (nodeAttr node attr) :: Double

---------- Helper functions ----------

nodeName :: Node -> String
nodeName node = unpack $ name node

nodeContentString :: Node -> String
nodeContentString node = foldl (\a b -> a ++ (extractText b)) "" (contents node)
  where extractText content = case content of
          Text bs -> unpack bs
          _       -> ""

nodeAttr :: Node -> String -> String
nodeAttr node attr = maybe "" (\v -> (unpack $ snd v)) $
  find (\n -> (fst n == pack attr)) (attributes node)
