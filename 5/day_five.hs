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


applyProcedure :: Procedure -> State -> State
applyProcedure (Procedure amt from to) (State state) = 
  if amt > 0 then
  let (popped_state, popped) = popAt from state in 
    let appended_state = appendAt to popped popped_state in
      if (not $ null  (state !! (from - 1))) then
      applyProcedure (Procedure (amt - 1) from to) (State appended_state)
      else 
      error $ show (Procedure amt from to) ++ " " ++ show (State state)
  else State state
            

applyProcedures :: State -> [Procedure] -> State
applyProcedures = foldr applyProcedure 


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
  print $ applyProcedure procedure unwrapped_state 
  return $ applyProcedure procedure unwrapped_state 
    
    

main :: IO ()
main = do
  -- crates <- readFile "sample-crates"
  -- procedures <- readFile "sample-procedures"
  crates <- readFile "crates"
  procedures <- readFile "procedures"

  let state = parseState $ removeCarriage crates
  let procedure_lines = map toProcedure $ lines $ removeCarriage procedures

  print state
  
  end <- foldl traceProcedure (return state) procedure_lines

  print $ showTop end
  
  -- print $ take 10 procedure_lines


  -- print $ showTop $ applyProcedures state procedure_lines

  return ()
