module Main where

import Data.List (find)

unwrap ::  String -> Maybe a -> a
unwrap e a = case a of 
  Nothing -> error e
  Just a -> a

getPriority :: Char -> Integer
getPriority item = snd . (unwrap "Priority") $ find (\(char, _) -> char == item) (zip (['a'..'z'] ++ ['A'..'Z']) ([1..]))

-- toCompartments :: String -> (String, String)
-- toCompartments s = splitAt ((length s) `div` 2) s

findBoth :: ([Char] , [Char]) -> Char
findBoth ((head:tail), search) = 
  case find (== head) search of 
  Just a -> a 
  Nothing -> findBoth (tail, search)

allChar :: [Maybe Char] -> Maybe Char 
allChar [char] = char
allChar (head:tail) =
  case head of 
  Nothing -> Nothing
  Just char -> case (allChar tail) of 
    Just sec_char -> if sec_char == char 
      then Just char 
      else Nothing
    Nothing -> Nothing



findAll :: [String] -> Char 
findAll ([]:_) = error "Findall failed to find all inputs"
findAll ((head:tail):searches) = 
  case allChar $ map (find (== head)) searches of 
  Nothing -> findAll (tail:searches)
  Just a -> a


-- [[S,S,S],[S,S,S]]
chunk :: [String] -> [[String]]
chunk [] = []
chunk a = (chunk (drop 3 a)) ++ [(take 3 a)]


totalSum :: String -> Integer
totalSum = sum . map (getPriority . findAll) .  chunk . lines 

main :: IO ()
main = do
  input <- readFile "input"
  print $ totalSum input
  return ()
