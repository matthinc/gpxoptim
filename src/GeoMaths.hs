module GeoMaths where

import Types
import qualified Data.Set as Set

-- Minimum / maximum values for latitude and longitude
latRange = (-90.0, 90)
lngRange = (-180, 180)

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

-- | Get bounds (min lat, min lng, max lat, max lng) of a segment
segmentBounds :: Segment -> Bounds
segmentBounds segment = walkPoints (points segment) $ Bounds ((snd latRange), (snd lngRange), (fst latRange), (fst lngRange))
  where walkPoints (p:ps) bounds =
            walkPoints ps (mergeBounds bounds $ Bounds (lat p, lng p, lat p, lng p))
        walkPoints [] bounds = bounds
        
-- | Get bounds of gpx route
gpxBounds :: GPX -> Bounds
gpxBounds gpx = walkSegments (segments gpx) $ Bounds  ((snd latRange), (snd lngRange), (fst latRange), (fst lngRange))
  where walkSegments (p:ps) bounds =
            walkSegments ps (mergeBounds bounds $ segmentBounds p)
        walkSegments [] bounds = bounds

-- | Merge two bound objects
mergeBounds :: Bounds -> Bounds -> Bounds
mergeBounds a b = let Bounds (aLatMin, aLngMin, aLatMax, aLngMax) = a
                      Bounds (bLatMin, bLngMin, bLatMax, bLngMax) = b in
                    Bounds (min aLatMin bLatMin, min aLngMin bLngMin, max aLatMax bLatMax, max aLngMax bLngMax)

-- | Scale to grid
scaleToGrid :: GPX -> Int -> Int -> Set.Set (Int, Int)
scaleToGrid gpx width height = foldl (\a b -> (pointToGrid b a)) Set.empty getAllPoints
  where pointToGrid p set = let Bounds (latMin, lngMin, latMax, lngMax) = gpxBounds gpx
                                x = floor $ (( (lng p) - lngMin) / (lngMax - lngMin)) * (fromIntegral width)
                                y = floor $ (( (lat p) - latMin) / (latMax - latMin)) * (fromIntegral height) in
                              Set.insert (x, y) set
        getAllPoints = foldl (\a b -> a ++ (points b)) [] (segments gpx)

-- | distance at segment[p] -> segment[p + 1].
distanceAt :: Segment -> Int -> Double
distanceAt segment pos
  | length ps < pos + 2 = 0.0
  | otherwise           = (ps !! pos) `distanceTo` (ps !! (pos + 1))
    where ps = points segment
