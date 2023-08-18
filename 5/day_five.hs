-- This module works on the crate diagrom split from the procedure list
--
-- Do it by hand it'll be quick

module Main where

import Control.Exception
import Utils (split, removeCarriage)

data State = State [String] deriving (Show)

--                         Tries   From    To
data Procedure = Procedure Integer Int Int deriving (Show)

fmtState :: State -> String
fmtState (State strings) = map head strings

toProcedure :: String -> Procedure 
toProcedure s = let s_words = split ' ' s in 
  Procedure (read $ s_words !! 1) (read $ s_words !! 3) (read $ s_words !! 5)


insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as 

appendAt :: Int -> Char -> [String] -> [String]
appendAt i c strings = map (\(string, current_index) -> if current_index == i then c:string else string ) $ zip strings [1..]

popAt :: Int -> [String] -> ([String] , Char)
popAt i strings = (map (\(string, current_index) -> if current_index == i then tail string else string) enum_strings, foldr (\(string, current_index) acc -> if current_index == i then head string else acc) ' ' enum_strings)
  where enum_strings = zip strings [1..]


applyProcedure :: State -> Procedure -> State
applyProcedure  (State state) (Procedure amt from to)= 
  if amt > 0 then
  let (popped_state, popped) = popAt from state in 
    let appended_state = appendAt to popped popped_state in
      if (not $ null  (state !! (from - 1))) then
      applyProcedure  (State appended_state) (Procedure (amt - 1) from to)
      else 
      error $ show (Procedure amt from to) ++ " " ++ show (State state)
  else State state
            

crateToLine :: Int -> Int
crateToLine c = 1 + c * 4

lineToCrate :: Int -> Int 
lineToCrate c = (c - 1) `div` 4

parseState :: String -> State 
parseState s =  let s_lines = lines (filter (/= '\r') s) in
  State $ map (filter (/= ' ') . (\crate_number -> foldr (\line acc -> (line !! (crateToLine crate_number)):acc) "" s_lines)) [0..(lineToCrate (length (s_lines !! 0)))] 

showTop :: State -> String
showTop (State s) = foldr (\(head:_) acc -> head:acc) "" s

traceProcedure :: IO State -> Procedure -> IO State 
traceProcedure state procedure = do
  unwrapped_state <- state
  print procedure
  print $ applyProcedure unwrapped_state procedure 
  return $ applyProcedure unwrapped_state procedure 
    
    

main :: IO ()
main = do
  -- crates <- readFile "sample-crates"
  -- procedures <- readFile "sample-procedures"
  crates <- readFile "crates"
  procedures <- readFile "procedures"

  let state =  State ["PZMTRCN","ZBSTND","GTCFRQHM","ZRG","HRNZ","DLZPWSHF","MGCRZDW","QZWHLFJS","NWPQS"] -- dump of parseState $ removeCarriage crates
  let procedure_lines = [Procedure 7 6 8,Procedure 5 2 6,Procedure 2 4 1,Procedure 1 4 5,Procedure 5 7 6,Procedure 7 6 3,Procedure 5 9 2,Procedure 6 2 3,Procedure 2 7 9,Procedure 20 3 1,Procedure 11 1 6,Procedure 1 9 8,Procedure 3 8 2,Procedure 8 1 5,Procedure 10 8 4,Procedure 7 6 4,Procedure 1 8 3,Procedure 8 1 7,Procedure 16 4 8,Procedure 1 9 8,Procedure 1 5 2,Procedure 4 7 4,Procedure 5 6 7,Procedure 1 6 1,Procedure 8 7 4,Procedure 1 6 9,Procedure 12 4 5,Procedure 3 2 5,Procedure 1 6 2,Procedure 1 3 7,Procedure 1 3 2,Procedure 1 9 3,Procedure 1 7 8,Procedure 1 7 5,Procedure 1 3 2,Procedure 4 5 7,Procedure 5 5 7,Procedure 1 4 3,Procedure 1 3 9,Procedure 3 1 8,Procedure 1 9 1,Procedure 2 2 1,Procedure 2 2 7,Procedure 8 8 1,Procedure 3 5 2,Procedure 8 7 5,Procedure 7 1 3,Procedure 3 1 7,Procedure 1 1 5,Procedure 1 3 7,Procedure 7 5 8,Procedure 2 2 8,Procedure 1 3 2,Procedure 1 2 4,Procedure 1 4 8,Procedure 13 8 1,Procedure 13 5 9,Procedure 2 5 2,Procedure 7 9 3,Procedure 12 8 3,Procedure 4 9 3,Procedure 1 3 4,Procedure 2 2 3,Procedure 1 1 6,Procedure 1 2 3,Procedure 1 5 9,Procedure 7 7 4,Procedure 10 1 8,Procedure 1 1 4,Procedure 1 9 5,Procedure 2 5 1,Procedure 1 6 5,Procedure 3 8 9,Procedure 5 4 3,Procedure 4 4 1,Procedure 7 1 6,Procedure 2 5 7,Procedure 35 3 4,Procedure 4 9 1,Procedure 19 4 8,Procedure 1 7 6,Procedure 1 9 2,Procedure 10 4 5,Procedure 2 4 7,Procedure 3 4 3,Procedure 1 2 8,Procedure 1 1 9,Procedure 3 3 6,Procedure 4 8 6,Procedure 4 5 2,Procedure 2 8 3,Procedure 3 5 9,Procedure 12 6 1,Procedure 8 8 6,Procedure 2 9 1,Procedure 1 4 1,Procedure 1 3 8,Procedure 3 7 8,Procedure 2 9 7,Procedure 1 6 7,Procedure 10 6 8,Procedure 4 2 5,Procedure 1 3 7,Procedure 7 5 7,Procedure 13 8 1,Procedure 29 1 4,Procedure 8 7 8,Procedure 1 1 3,Procedure 3 7 6,Procedure 1 1 9,Procedure 15 4 1,Procedure 1 3 6,Procedure 10 1 6,Procedure 10 6 7,Procedure 1 4 9,Procedure 1 9 1,Procedure 1 9 7,Procedure 6 7 8,Procedure 1 1 6,Procedure 5 6 5,Procedure 21 8 9,Procedure 5 1 9,Procedure 2 9 5,Procedure 3 5 6,Procedure 3 7 9,Procedure 4 4 6,Procedure 6 8 7,Procedure 6 6 3,Procedure 2 7 9,Procedure 1 7 2,Procedure 6 3 2,Procedure 1 6 4,Procedure 4 5 9,Procedure 1 4 5,Procedure 9 4 6,Procedure 7 6 4,Procedure 10 9 2,Procedure 5 7 5,Procedure 10 2 7,Procedure 2 5 4,Procedure 2 5 9,Procedure 4 9 4,Procedure 1 8 6,Procedure 7 7 2,Procedure 1 5 4,Procedure 2 7 1,Procedure 1 5 7,Procedure 3 6 2,Procedure 4 4 5,Procedure 1 2 7,Procedure 10 4 7,Procedure 3 7 3,Procedure 17 9 4,Procedure 1 1 4,Procedure 1 1 5,Procedure 5 2 7,Procedure 1 9 2,Procedure 5 4 8,Procedure 2 9 7,Procedure 4 8 1,Procedure 3 4 8,Procedure 1 2 5,Procedure 1 9 2,Procedure 6 4 8,Procedure 3 7 5,Procedure 1 4 9,Procedure 1 9 1,Procedure 3 1 9,Procedure 4 8 5,Procedure 2 9 8,Procedure 4 2 5,Procedure 8 7 2,Procedure 5 8 5,Procedure 2 7 8,Procedure 1 3 5,Procedure 1 1 2,Procedure 1 1 6,Procedure 2 3 6,Procedure 5 2 8,Procedure 4 7 1,Procedure 7 8 5,Procedure 1 1 5,Procedure 3 8 3,Procedure 1 9 3,Procedure 7 2 3,Procedure 2 2 8,Procedure 2 4 8,Procedure 1 8 5,Procedure 1 1 4,Procedure 2 4 7,Procedure 2 7 1,Procedure 3 2 3,Procedure 3 5 2,Procedure 1 8 3,Procedure 3 3 2,Procedure 5 2 1,Procedure 17 5 8,Procedure 9 8 1,Procedure 11 3 5,Procedure 8 8 5,Procedure 2 8 5,Procedure 16 1 4,Procedure 13 4 7,Procedure 6 5 2,Procedure 2 4 8,Procedure 5 7 9,Procedure 2 1 2,Procedure 7 7 1,Procedure 1 1 4,Procedure 1 9 8,Procedure 7 2 8,Procedure 1 4 7,Procedure 2 9 4,Procedure 1 4 1,Procedure 1 3 5,Procedure 2 9 8,Procedure 11 8 7,Procedure 2 6 5,Procedure 1 6 9,Procedure 1 1 9,Procedure 1 9 1,Procedure 4 1 4,Procedure 2 1 8,Procedure 1 1 2,Procedure 1 9 5,Procedure 2 4 3,Procedure 2 2 7,Procedure 2 3 9,Procedure 1 9 1,Procedure 1 9 1,Procedure 5 5 1,Procedure 19 5 6,Procedure 5 1 4,Procedure 1 2 9,Procedure 1 1 3,Procedure 7 5 8,Procedure 1 3 6,Procedure 8 7 3,Procedure 7 4 8,Procedure 3 8 5,Procedure 1 4 1,Procedure 1 9 4,Procedure 1 4 9,Procedure 1 5 2,Procedure 2 5 6,Procedure 2 8 2,Procedure 7 8 1,Procedure 1 1 7,Procedure 3 6 9,Procedure 2 3 2,Procedure 1 2 1,Procedure 1 8 7,Procedure 2 9 6,Procedure 2 9 5,Procedure 1 5 6,Procedure 1 2 8,Procedure 2 1 7,Procedure 1 4 3,Procedure 3 2 5,Procedure 7 1 3,Procedure 10 3 4,Procedure 3 5 4,Procedure 1 3 8,Procedure 3 3 2,Procedure 1 8 1,Procedure 1 1 3,Procedure 3 8 3,Procedure 5 4 6,Procedure 1 2 3,Procedure 4 6 4,Procedure 1 5 7,Procedure 4 3 4,Procedure 1 2 8,Procedure 12 7 6,Procedure 1 8 2,Procedure 2 2 7,Procedure 1 8 4,Procedure 23 6 3,Procedure 14 3 6,Procedure 15 4 6,Procedure 1 8 6,Procedure 10 3 7,Procedure 2 4 2,Procedure 11 7 8,Procedure 2 2 6,Procedure 44 6 9,Procedure 21 9 3,Procedure 12 3 6,Procedure 1 7 4,Procedure 1 4 7,Procedure 9 3 2,Procedure 2 8 6,Procedure 3 2 4,Procedure 17 9 1,Procedure 3 4 6,Procedure 2 2 9,Procedure 4 9 2,Procedure 10 6 9,Procedure 1 7 6,Procedure 4 9 5,Procedure 4 2 4,Procedure 14 1 5,Procedure 4 4 3,Procedure 3 2 9,Procedure 9 9 7,Procedure 1 2 5,Procedure 9 8 5,Procedure 8 7 2,Procedure 4 3 8,Procedure 5 6 2,Procedure 3 1 6,Procedure 1 7 1,Procedure 4 2 4,Procedure 3 6 4,Procedure 3 8 3,Procedure 13 5 2,Procedure 2 3 5,Procedure 12 5 9,Procedure 1 3 5,Procedure 1 5 9,Procedure 1 8 3,Procedure 4 9 5,Procedure 6 4 5,Procedure 12 9 7,Procedure 1 9 3,Procedure 1 3 2,Procedure 12 5 6,Procedure 12 7 2,Procedure 1 3 7,Procedure 1 4 8,Procedure 33 2 8,Procedure 1 7 5,Procedure 1 1 2,Procedure 4 5 4,Procedure 3 2 5,Procedure 34 8 6,Procedure 1 4 3,Procedure 1 5 7,Procedure 1 7 5,Procedure 3 4 9,Procedure 2 9 7,Procedure 1 9 4,Procedure 1 3 7,Procedure 1 5 8,Procedure 1 5 1,Procedure 1 5 7,Procedure 1 4 8,Procedure 1 1 4,Procedure 1 4 2,Procedure 3 7 5,Procedure 2 8 5,Procedure 1 2 8,Procedure 4 6 2,Procedure 1 8 6,Procedure 1 7 9,Procedure 29 6 7,Procedure 4 2 3,Procedure 2 5 8,Procedure 1 9 5,Procedure 2 8 1,Procedure 23 7 5,Procedure 2 6 1,Procedure 23 5 6,Procedure 1 3 6,Procedure 4 5 9,Procedure 2 1 3,Procedure 5 3 8,Procedure 2 6 5,Procedure 2 1 4,Procedure 1 9 8,Procedure 1 9 1,Procedure 1 4 6,Procedure 2 5 6,Procedure 6 7 8,Procedure 2 9 2,Procedure 18 6 5,Procedure 21 6 4,Procedure 1 1 6,Procedure 2 6 7,Procedure 2 7 9,Procedure 2 2 8,Procedure 7 4 3,Procedure 12 5 3,Procedure 1 9 5,Procedure 1 9 4,Procedure 6 5 2,Procedure 17 3 4,Procedure 3 4 3,Procedure 1 2 4,Procedure 5 2 8,Procedure 1 5 8,Procedure 19 8 7,Procedure 1 3 6,Procedure 1 8 4,Procedure 1 6 1,Procedure 15 4 6,Procedure 1 1 4,Procedure 3 3 5,Procedure 4 6 7,Procedure 1 4 7,Procedure 10 6 7,Procedure 16 4 5,Procedure 24 7 2,Procedure 8 7 8,Procedure 1 4 2,Procedure 6 8 7,Procedure 1 8 7,Procedure 1 6 9,Procedure 14 5 4,Procedure 9 7 8,Procedure 4 5 1,Procedure 2 1 5,Procedure 3 8 6,Procedure 2 6 9,Procedure 2 2 8,Procedure 6 2 7,Procedure 3 4 6,Procedure 1 3 4,Procedure 3 5 7,Procedure 1 6 9,Procedure 5 7 2,Procedure 4 9 1,Procedure 1 7 9,Procedure 9 8 4,Procedure 5 1 2,Procedure 2 6 1,Procedure 6 4 7,Procedure 1 7 3,Procedure 1 3 9,Procedure 1 9 7,Procedure 1 6 7,Procedure 9 4 5,Procedure 7 7 9,Procedure 3 7 5,Procedure 1 9 2,Procedure 6 9 8,Procedure 4 4 5,Procedure 1 4 2,Procedure 1 4 2,Procedure 2 1 2,Procedure 1 9 8,Procedure 10 2 4,Procedure 8 2 7,Procedure 12 2 9,Procedure 6 7 4,Procedure 1 1 2,Procedure 8 9 8,Procedure 7 5 1,Procedure 9 4 3,Procedure 14 8 4,Procedure 1 8 4,Procedure 1 1 5,Procedure 1 5 2,Procedure 3 2 4,Procedure 1 7 1,Procedure 1 7 3,Procedure 2 1 7,Procedure 3 5 7,Procedure 2 7 6,Procedure 1 6 5,Procedure 3 7 1,Procedure 1 6 8,Procedure 1 8 7,Procedure 1 3 6,Procedure 1 7 1,Procedure 4 1 4,Procedure 6 3 2,Procedure 3 1 2,Procedure 3 3 6,Procedure 3 2 6,Procedure 6 6 5,Procedure 1 1 4,Procedure 1 9 6,Procedure 5 2 1,Procedure 3 1 2,Procedure 2 9 8,Procedure 3 1 5,Procedure 1 9 7,Procedure 25 4 1,Procedure 1 1 7,Procedure 2 8 3,Procedure 13 1 9,Procedure 2 3 5,Procedure 8 5 9,Procedure 4 2 1,Procedure 2 6 7,Procedure 10 5 9,Procedure 4 7 2,Procedure 2 2 3,Procedure 9 9 2,Procedure 4 4 5,Procedure 4 5 4,Procedure 5 1 4,Procedure 10 4 5,Procedure 22 9 1,Procedure 2 2 7,Procedure 3 2 1,Procedure 6 2 6,Procedure 1 7 1,Procedure 10 5 7,Procedure 15 1 4,Procedure 13 1 5,Procedure 3 6 8,Procedure 1 8 9]
  
  print $ showTop $ foldl applyProcedure state procedure_lines

  return ()
