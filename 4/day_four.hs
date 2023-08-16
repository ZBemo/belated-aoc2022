module Main where

import Data.List (elemIndex)

data Range = Range Integer Integer deriving (Show)

removeCarriage = filter (/= '\r')

contains :: Range -> Range -> Bool
contains (Range i11 i12) (Range i21 i22) = i11 <= i21 && i12 >= i22

unwrap :: Maybe a -> a 
unwrap a = case a of 
 Nothing -> error "Unwrap called on Nothing"
 Just a -> a

splitComma :: String -> (String, String)
splitComma s = let split_index = unwrap (elemIndex ',' s) in
  let (fst, snd) = splitAt split_index s in
   (fst, (drop 1 snd))

readRange :: String -> Range
readRange s = let split_index = unwrap (elemIndex '-' s) in 
  let (fst, snd) =  splitAt split_index s in
    Range (read fst) (read (drop 1 snd))
  

readRangePair :: (String,String) -> (Range, Range)
readRangePair (fst,snd) = (readRange fst, readRange snd)

stringToRanges :: String -> [(Range, Range)]
stringToRanges = (map readRangePair) . (map splitComma) . lines . removeCarriage

getOverlap :: [(Range, Range)] -> [Bool]
getOverlap = map eitherContains
  where eitherContains = \(fst, snd) -> (contains fst snd) || (contains snd fst)

main :: IO ()
main = do
  input <- readFile "input"
  print $ sum . map (\x -> if x then 1 else 0) . getOverlap . stringToRanges $ input
  return ()
