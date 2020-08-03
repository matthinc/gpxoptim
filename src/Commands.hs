module Commands where

import Text.ParserCombinators.Parsec
import Types

import TrimStart

-- | Execute commands string
runCommands :: String -> GPX -> GPX
runCommands commands gpx = foldl (\a b -> (runCommand a b)) gpx cmds
  where cmds = case (parse parseCommands "" commands) of
          Right c -> c
          Left _  -> []

runCommand :: GPX -> Command -> GPX
runCommand gpx command = case command of
  CommandTrimStart len -> trimStart gpx len
  _                    -> gpx

---------- Parse ----------

parseCommands :: Parser [Command]
parseCommands = many1 parseCommandWithSemicolon

-- | Parses "trim_start (10km); "
parseCommandWithSemicolon :: Parser Command
parseCommandWithSemicolon = do
  cmd <- parseCommand
  spaces
  char ';'
  spaces
  pure cmd

-- | Parses "trim_start (10km)"
parseCommand :: Parser Command
parseCommand = (do
  try (do
    string "trim_start"
    spaces
    distance <- parseArgumentsList parseDistance
    pure $ CommandTrimStart distance)
  <|>
  try (do
    string "trim_end"
    spaces
    distance <- parseArgumentsList parseDistance
    pure $ CommandTrimEnd distance))

-- | Parses "(" ... ")"
parseArgumentsList :: Parser a -> Parser a
parseArgumentsList argumentsParser = do
  spaces
  char '('
  spaces
  result <- argumentsParser
  spaces
  char ')'
  pure result

-- | Parses "10km" or "10m"
parseDistance :: Parser Integer
parseDistance = do
          try parseMeter
          <|>
          try parseKilometer

-- | Parses "10m"
parseMeter :: Parser Integer
parseMeter = do
  value <- read <$> many1 digit
  char 'm'
  pure value

-- | Parses "10km"
parseKilometer :: Parser Integer
parseKilometer = do
  value <- read <$> many1 digit
  string "km"
  pure $ value * 1000

