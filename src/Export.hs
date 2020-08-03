module Export where

import Types

gpxToXml :: GPX -> String
gpxToXml gpx = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><gpx><trk>" ++ segmentsToXml (segments gpx)  ++"</trk></gpx>"

segmentsToXml :: [Segment] -> String
segmentsToXml segments = concat $ map segmentToXml segments

segmentToXml :: Segment -> String
segmentToXml segment = "<trkseg>" ++ pointsToXml (points segment) ++ "</trkseg>"

pointsToXml :: [Point] -> String
pointsToXml points = concat $ map pointToXml points

pointToXml :: Point -> String
pointToXml point =  "<trkpt lat=\"" ++ (show $ lat point) ++ "\" lon=\"" ++ (show $ lng point) ++ "\"></trkpt>"


