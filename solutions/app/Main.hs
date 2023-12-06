module Main (main) where

import qualified Solutions.Year2023.Day1 as Day1
import qualified Solutions.Year2023.Day2 as Day2

main :: IO ()
main = do
  putStrLn "Solutions:"
  putStrLn ""

  inputDay1 <- readFile "inputs/2023/day1/input.txt"
  printSolution "2023 Day 1 Problem 1" $ Day1.problem1 inputDay1
  printSolution "2023 Day 1 Problem 2" $ Day1.problem2 inputDay1

  inputDay2 <- readFile "inputs/2023/day2/input.txt"
  printSolution "2023 Day 2 Problem 1" $ Day2.problem1 inputDay2
  printSolution "2023 Day 2 Problem 2" $ Day2.problem2 inputDay2

  putStrLn "End Solutions"

printSolution :: Show a => String -> a -> IO ()
printSolution title solution = do
  putStrLn $ "Problem: " ++ title
  putStrLn $ "solution: " ++ show solution
  putStrLn ""
