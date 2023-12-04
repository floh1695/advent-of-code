module Main (main) where

import qualified Solutions.Year2023.Day1 as Day1

main :: IO ()
main = do
  printSolution "2023 Day 1 Problem 1" Day1.doProblem1

printSolution :: Show a => String -> IO a -> IO ()
printSolution title solutionAction = do
  solution <- solutionAction

  putStrLn $ "Problem: " ++ title
  putStrLn $ "solution: " ++ show solution
  putStrLn ""
