module Main (main) where

import qualified Solutions.Year2023.Day1 as Day1
import qualified Solutions.Year2023.Day2 as Day2

main :: IO ()
main = do
  putStrLn "Solutions:"
  putStrLn ""

  doDay 2023 1 Day1.problem1 Day1.problem2
  doDay 2023 2 Day2.problem1 Day2.problem2

  putStrLn "End Solutions"

doDay :: Show a => Int -> Int -> (String -> a) -> (String -> a) -> IO ()
doDay year day problem1 problem2 = do
  let title = unwords ["Year", show year, "Day", show day]
  input <- readFile $ mconcat ["inputs/year", show year, "/day", show day, "/input.txt"]
  printSolution (title ++ " Problem 1") $ problem1 input
  printSolution (title ++ " Problem 2") $ problem2 input

printSolution :: Show a => String -> a -> IO ()
printSolution title solution = do
  putStrLn $ "Problem: " ++ title
  putStrLn $ "solution: " ++ show solution
  putStrLn ""
