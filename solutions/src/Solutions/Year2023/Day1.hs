{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Solutions.Year2023.Day1
  ( problem1
  , problem2
  ) where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.List as List

problem1 :: String -> Int
problem1 input = solution
  where
    inputs   = lines input
    digits   = map (filter Char.isDigit) inputs
    numbers  = map (\x -> read [head x, last x]) digits
    solution = sum numbers

problem2 :: String -> Int
problem2 input = solution
  where
    inputs   = lines input
    go :: [String] -> [String] -> String
    go taken []               = unlines . reverse $ taken
    go taken (check:checking) = go (check' : taken) checking
      where
        check' = ensureDigits check
    input'   = go [] inputs
    solution = problem1 input'

ensureDigits :: String -> String
ensureDigits = ensureLastDigit . ensureFirstDigit

ensureFirstDigit :: String -> String
ensureFirstDigit = ensureDigit wordDigitPairs

ensureLastDigit :: String -> String
ensureLastDigit = reverse . ensureDigit (map (Bifunctor.first reverse) wordDigitPairs) . reverse

ensureDigit :: [(String, Char)] -> String -> String
ensureDigit wordDigits input = result
  where
    go :: String -> String -> String
    go taken "" = taken
    go taken checking
      | Char.isDigit . head $ checking  =  reverse taken ++ checking
      | checking /= checking'           =  reverse taken ++ checking'
      | otherwise                       =  go (head checking' : taken) (tail checking')
      where
        checking' = replaceNextWordWithDigit wordDigits checking
    result = go "" input

replaceNextWordWithDigit :: [(String, Char)] -> String -> String
replaceNextWordWithDigit wordDigits input = replaced
  where
    replacers = map (uncurry replaceNextWordWithDigit') wordDigits
    replacer  = foldl (.) id replacers
    replaced  = replacer input

replaceNextWordWithDigit' :: String -> Char -> String -> String
replaceNextWordWithDigit' word digit input =
  if List.isPrefixOf word input
  then digit : drop (length word) input
  else input

wordDigitPairs :: [(String, Char)]
wordDigitPairs =
  [ ("one",   '1')
  , ("two",   '2')
  , ("three", '3')
  , ("four",  '4')
  , ("five",  '5')
  , ("six",   '6')
  , ("seven", '7')
  , ("eight", '8')
  , ("nine",  '9')
  ]
