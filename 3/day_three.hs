module Main where

import Data.List (find)

unwrap ::  String -> Maybe a -> a
unwrap e a = case a of 
  Nothing -> error e
  Just a -> a

getPriority :: Char -> Integer
getPriority item = snd . (unwrap "Priority") $ find (\(char, _) -> char == item) (zip (['a'..'z'] ++ ['A'..'Z']) ([1..]))

toCompartments :: String -> (String, String)
toCompartments s = splitAt ((length s) `div` 2) s

findBoth :: ([Char] , [Char]) -> Char
findBoth ((head:tail), search) = 
  case find (== head) search of 
  Just a -> a 
  Nothing -> findBoth (tail, search)



totalSum :: String -> Integer
totalSum = sum . map (getPriority . findBoth . toCompartments) . lines 

main :: IO ()
main = do
  input <- readFile "input"
  print $ totalSum input
  return ()
