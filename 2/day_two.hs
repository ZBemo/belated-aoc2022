module Main where

import Data.List (find)

data Move = Rock | Paper | Scissor deriving (Show, Eq)
data Outcome = Draw | Win | Loss deriving (Show, Eq)

scoreMove :: Move -> Integer
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissor = 3

beatslist = [(Paper, Rock), (Scissor,Paper), (Rock, Scissor)]

beats :: (Move, Move) -> Bool
beats moves = case find (== moves) beatslist of
  Just _ -> True
  Nothing -> False



outcome :: (Move, Move) -> Integer 
outcome (opp, us)  -- opp = Rock, us = Paper
 | beats (us, opp) =  6
 | us == opp = 3
 | otherwise = 0

score :: (Move, Move) -> Integer 
score (opp, us) = (scoreMove us) + outcome (opp,us)

decodeUs :: Char -> Outcome
decodeUs 'X' = Loss
decodeUs 'Y' = Draw
decodeUs 'Z' = Win

decodeOpp :: Char -> Move
decodeOpp 'A' = Rock
decodeOpp 'B' = Paper
decodeOpp 'C' = Scissor

decodeOutcome :: (Move, Outcome) -> (Move, Move)
decodeOutcome (m, Draw) = (m,m)
decodeOutcome (m, Win) = case find ((== m) . snd) beatslist of 
  Just (win, _) -> (m, win)
  Nothing -> error "Couldn't decode outcome"
decodeOutcome (m, Loss) = case find ((== m) . fst) beatslist of 
  Just (_, loss) -> (m, loss)
  Nothing -> error "Couldn't decode outcome"



decodeMove :: (Char, Char) -> (Move, Move)
decodeMove (opponent, us) = 
    decodeOutcome (decodeOpp opponent, decodeUs us)

calculate :: String -> Integer
calculate input = 
  sum $ map (\x -> score $ (decodeMove (x !! 0, x !! 2))) $ lines input 


main :: IO ()
main = do
  input <- readFile "input"
  print $ calculate input
  return ()
