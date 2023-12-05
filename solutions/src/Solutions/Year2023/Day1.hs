module Solutions.Year2023.Day1
  ( doProblem1
  , problem1
  ) where

import qualified Data.Char as Char

doProblem1 :: IO Int
doProblem1 = do
  input <- readFile "inputs/2023/day1/input.txt"
  return $ problem1 input

problem1 :: String -> Int
problem1 input = solution
  where
    inputs   = lines input
    digits   = map (filter Char.isDigit) inputs
    numbers  = map (\x -> read [head x, last x]) digits
    solution = sum numbers
