module Types where

-- | Represents a complete .GPX file
data GPX = GPX { segments :: [Segment],
                 trackName :: Maybe String }
             deriving (Show, Eq)

-- | Route segment
data Segment = Segment { points :: [Point] }
             deriving (Show, Eq)

-- | Single point
data Point = Point { lat :: Double,
                     lng :: Double }
             deriving (Show, Eq)
