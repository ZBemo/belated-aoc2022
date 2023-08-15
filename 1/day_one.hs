
module Main where

import Data.List (groupBy)

removeCarriage :: String -> String
removeCarriage = filter (not . (== '\r'))

splitEmpty :: String -> [[String]]
splitEmpty = map (filter (not . null)) . groupBy (\_ x -> not $ null x) . lines 
  
sums :: [[String]] -> [Integer]
sums = map (sum . (map read))

-- maximum :: Ord a => [a] -> a
-- maximum list = foldr 
--   (\acc elem -> if acc < elem then elem else acc) 
--   (head list) (tail list)

main :: IO ()
main = do
  input <- readFile "input"  
  print $ maximum . sums . splitEmpty . removeCarriage $ input
  return ()
