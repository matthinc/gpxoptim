module TrimStart where

import Types
import GeoMaths

trimStart :: GPX -> Integer -> GPX
trimStart gpx distance = gpx { segments = trimSegments (segments gpx) (fromIntegral distance) }
  where findSplitPosition segment pointer len currentLen =
          if pointer >= (length $ points segment) - 1 || currentLen > len
             then pointer
             else findSplitPosition segment (pointer + 1) len $ currentLen + (distanceAt segment pointer)
        trimSegment segment trim = Segment $ drop (findSplitPosition segment 0 trim 0) (points segment)
        trimSegments segments trim = let s:ss = segments in (trimSegment s trim):ss
