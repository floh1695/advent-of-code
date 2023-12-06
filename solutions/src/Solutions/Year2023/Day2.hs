module Solutions.Year2023.Day2
  ( problem1
  , problem2
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split

problem1 :: String -> Int
problem1 input = solution
  where
    games       = parseGames input
    test        = newHand 12 13 14
    passing     = filter (testGame test) games
    identifiers = map identifier passing
    solution    = sum identifiers

problem2 :: String -> Int
problem2 input = solution
  where
    games    = parseGames input
    solution = sum $ map (handProduct . minimumHandForGame) games

data Game = Game
  { identifier :: Int
  , hands      :: [Hand]
  }
  deriving (Show)

newGame :: Int -> [Hand] -> Game
newGame identifier' hands' = Game
  { identifier = identifier'
  , hands      = hands'
  }

parseGame :: String -> Game
parseGame input = newGame (read identifier') hands'
  where
    atIdentifier = dropWhile (not . Char.isDigit) input
    identifier'  = takeWhile Char.isDigit atIdentifier
    atHands      = drop (length identifier' + length ": ") atIdentifier
    hands'       = map parseHand . Split.splitOn "; " $ atHands

parseGames :: String -> [Game]
parseGames input = map parseGame $ lines input

testGame :: Hand -> Game -> Bool
testGame test game = all (testHand test) $ hands game

minimumHandForGame :: Game -> Hand
minimumHandForGame game = foldl greatestHand emptyHand $ hands game

data Hand = Hand
  { red   :: Int
  , green :: Int
  , blue  :: Int
  }
  deriving (Show)

newHand :: Int -> Int -> Int -> Hand
newHand red' green' blue' = Hand
  { red   = red'
  , green = green'
  , blue  = blue'
  }

emptyHand :: Hand
emptyHand = newHand 0 0 0

parseHand :: String -> Hand
parseHand input = newHand red' green' blue'
  where
    cubeses    = map parseCubes . Split.splitOn ", " $ input
    forColor c = maybe 0 count . List.find ((== c) . color) $ cubeses
    red'       = forColor "red"
    green'     = forColor "green"
    blue'      = forColor "blue"

handCount :: Hand -> Int
handCount hand = sum $ map ($ hand) [red, green, blue]

testHand :: Hand -> Hand -> Bool
testHand test hand = all (\f -> f test >= f hand) [handCount, red, green, blue]

greatestHand :: Hand -> Hand -> Hand
greatestHand left right = newHand red' green' blue'
  where
    red'   = red left `max` red right
    green' = green left `max` green right
    blue'  = blue left `max` blue right

handProduct :: Hand -> Int
handProduct hand = product $ map ($ hand) [red, green, blue]

data Cubes = Cubes
  { color :: String
  , count :: Int
  }

newCubes :: String -> Int -> Cubes
newCubes color' count' = Cubes
  { color = color'
  , count = count'
  }

parseCubes :: String -> Cubes
parseCubes input = newCubes color' count'
  where
    tokens  = Split.splitOn " " input
    count'  = read $ head tokens
    color'  = last tokens
