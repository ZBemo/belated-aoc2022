-- This module works on the crate diagrom split from the procedure list
--
-- Do it by hand it'll be quick

module Main where

import Control.Exception
import Utils (split, removeCarriage)
import Init (state, procedure_lines, State(..), Procedure(..))

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

popAt2 :: Int -> Int -> [String] -> ([String] , String)
popAt2 i amt strings = (map (\(string, current_index) -> if current_index == i then drop amt string else string) enum_strings, foldr (\(string, current_index) acc -> if current_index == i then take amt string else acc) "" enum_strings)
  where enum_strings = zip strings [1..]

appendAt2 :: Int -> String -> [String] -> [String]
appendAt2 i to_append strings = map (\(string, current_index) -> if current_index == i then (to_append ++ string) else string) $ zip strings [1..]

applyProcedure2 :: State -> Procedure -> State
applyProcedure2  (State state) (Procedure amt from to) =  
  let (popped_state, popped) = popAt2 from amt state in 
    let appended_state = appendAt2 to popped popped_state in
      State appended_state
    


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

  print $ showTop $ foldl applyProcedure2 state procedure_lines

  return ()
