module GeoMaths where

import Types

-- | Distance between two points in meters
distanceTo :: Point -> Point -> Double
distanceTo p1 p2 = sdst * rEar
  where dLat = (lat p2) - (lat p1)
        dLng = (lng p2) - (lng p1)
        dLatRad = dLat * pi / 180
        dLngRad = dLng * pi / 180
        dist = sin(dLatRad/2)^2 + sin(dLngRad/2)^2 * cos(lat p1) * cos(lat p2)
        sdst = 2 * atan2 (sqrt dist) (sqrt (1 - dist))
        rEar = 6371000

-- | Calculate length of a segment
segmentLength :: Segment -> Double
segmentLength segment = pointArrayDist $ points segment
  where pointArrayDist [a]        = 0
        pointArrayDist [a, b]     = (a `distanceTo` b)
        pointArrayDist (a:b:rest) = (a `distanceTo` b) + pointArrayDist (b:rest)

-- | Total length of GPX route
gpxLength :: GPX -> Double
gpxLength gpx = foldl (\a b -> a + (segmentLength b)) 0 (segments gpx)
