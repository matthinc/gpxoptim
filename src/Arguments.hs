module Arguments where

import Text.ParserCombinators.Parsec
import Types

parseCommands :: Parser [Command]
parseCommands = many1 parseCommandWithSemicolon

-- | Parses "trim_start (10km); "
parseCommandWithSemicolon :: Parser Command
parseCommandWithSemicolon = do
  cmd <- parseCommand
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

