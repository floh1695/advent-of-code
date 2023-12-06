module Main (main) where

import qualified Solutions.Year2023.Day1 as Day1

main :: IO ()
main = do
  putStrLn "Solutions:"
  putStrLn ""

  day1Input <- readFile "inputs/2023/day1/input.txt"
  printSolution "2023 Day 1 Problem 1" $ Day1.problem1 day1Input
  printSolution "2023 Day 1 Problem 2" $ Day1.problem2 day1Input

  putStrLn "End Solutions"

printSolution :: Show a => String -> a -> IO ()
printSolution title solution = do
  putStrLn $ "Problem: " ++ title
  putStrLn $ "solution: " ++ show solution
  putStrLn ""
