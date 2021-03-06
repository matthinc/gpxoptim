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

-- | (min lat, min lng, max lat, max lng)
data Bounds = Bounds (Double, Double, Double, Double)
  deriving (Show, Eq)

-- | Command
data Command = CommandTrimStart Integer
             | CommandTrimEnd Integer
             deriving (Show, Eq)
