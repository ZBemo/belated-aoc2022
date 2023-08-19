module Main where

import Utils (removeCarriage)
import Data.Map.Strict  (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
 
allUnique :: String ->  Bool
allUnique = not . any (\c -> c > 1) . consolidate 
  where consolidate = foldr (Map.alter (Just . (+1) . (fromMaybe 0))) Map.empty
  
  

findUnique :: String -> Int -> Int
findUnique [] _ = error "Found no unique four length token"
findUnique s index = if allUnique $ take 14 s then index else findUnique (tail s) (index + 1)

findMarker :: String -> Int
findMarker s = (14 +) $ findUnique s 0

main :: IO ()
main = do
  input <-  (readFile "input" >>= return . removeCarriage)

  print $ findMarker input

  return ()
