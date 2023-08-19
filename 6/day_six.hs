module Main where

import Utils (removeCarriage)
import Data.Map.Strict  (Map)
import qualified Data.Map.Strict as Map
 
allUnique :: String ->  Bool
allUnique = not . any (\c -> c > 1) . consolidate Map.empty
  where consolidate map [] = map
        consolidate map (h:t) = consolidate (Map.alter (\x -> Just $ case x of Nothing -> 1; Just n -> (n + 1);) h map) t
  
  

findUnique :: String -> Int -> Int
findUnique [] _ = error "Found no unique four length token"
findUnique s index = if allUnique $ take 4 s then index else findUnique (tail s) (index + 1)

findMarker :: String -> Int
findMarker s = (4 +) $ findUnique s 0

main :: IO ()
main = do
  input <-  (readFile "input" >>= return . removeCarriage)

  print $ findMarker input

  return ()
