module Main where

import Utils (removeCarriage, split)
import Control.Exception (assert)
import Data.List (findIndex, find)
import Data.Maybe (fromJust)
import Data.Map.Strict  (Map)
import qualified Data.Map.Strict as Map

--                 Map name->node       Size
data Node = Dir (Map String Node) | File Int deriving (Show)
type ResolvedPath = [String]
-- type Folder = Map String Noe

size :: Map String Node -> Int
size = foldr addSize 0
  where 
    addSize n acc = acc + case n of 
      Dir f -> (size f)
      File size -> size 

fromDir :: Node -> (Map String Node)
fromDir n = case n of 
  File _ -> error "fromDir called on File"
  Dir m -> m


insertFile :: Node -> ResolvedPath -> (Map String Node) -> (Map String Node)
--                    keep old values. non-replacing lookup
insertFile n [f] fs = Map.insertWith (\new old -> old) f n fs
insertFile n (f:tail) fs = Map.alter (Just . Dir . (insertFile n tail) . fromDir . fromJust) f fs

recResolve :: [String] -> ResolvedPath
recResolve l = case findIndex (== "..") l of
  Just i -> let (first, second) = splitAt i l in recResolve $ (init first) ++ (tail second)
  Nothing -> l
  
-- go from a path with '..' to a list of directories
resolvePath :: String -> ResolvedPath
resolvePath = recResolve . split '/'

getNode :: ResolvedPath -> Node -> Node
getNode [head] fs = case fs of 
  File _ -> error $ "file system not deep enough at path" ++ head
  Dir map -> fromJust $ Map.lookup head map 
getNode (head:path) fs = case fs of 
  File _ -> error $ "file system not deep enough at path" ++ show path ++ show head ++ show fs
  Dir map -> getNode path . fromJust $ Map.lookup head map 
  
--             commands-queu CurrentPath  full filesystem     full filesystem
parseCommands :: [String] -> [String] -> (Map String Node) -> (Map String Node)
parseCommands [] _ fs = fs
parseCommands (h:t) pwd fs = 
  if head h == '$' then
    let stripped = words $ drop 2 h in
      if head stripped == "cd" then
      parseCommands t (pwd ++ (take 1 $ tail stripped)) $ insertFile (Dir Map.empty) (recResolve (pwd ++ (take 1 $ tail stripped))) fs 
      else 
      if head stripped == "ls" then 
        doLines t pwd fs 
      else  error $ "invalid cmd; " ++ h
  else 
    error "TODO"
  where 
  doLines [h] pwd fs = 
    if head h == '$' then parseCommands [h] pwd fs
    else let [disc,name] = words h in 
      if disc == "dir" then insertFile (Dir Map.empty) (recResolve (pwd ++ [name])) fs
      else insertFile (File $ read disc) (recResolve (pwd ++ [name])) fs
  doLines (h:t) pwd fs =
    if head h == '$' then parseCommands (h:t) pwd fs
    else let [disc,name] = words h in 
      if disc == "dir" then doLines t pwd $ insertFile (Dir Map.empty) (recResolve (pwd ++ [name])) fs
      else doLines t pwd $ insertFile (File $ read disc) (recResolve (pwd ++ [name])) fs

-- string parsing

findNode :: (Node -> Bool) -> Node -> [Node]
findNode f fs = case fs of 
  File s -> if f $ fs then [fs] else []
  Dir dir ->  if f fs then fs:sub else sub
    where sub = concat $ (map ((findNode f) . snd) $ Map.toList $ dir)


tree :: (String, Node) -> String
tree (name, node) = 
  case node of 
    File size -> (show size ++ " " ++ name ++ "\n")
    Dir dir -> ("\\ " ++ name ++ "\n" ++ foldr (\e acc -> acc ++ (tree e)) "" (Map.toList dir))


main :: IO ()
main = do
  input <- (readFile "real-input" >>= return . removeCarriage)

  let fs = getNode ["/"] $ (Dir $ parseCommands (lines input) [] Map.empty) 

  putStrLn $ foldr1 (++) $ map (tree . ("/",)) $ findNode (\node -> case node of Dir dir -> (size dir <= 100000); File _ -> False) fs
  print $ sum $ map (size . fromDir) $ findNode (\node -> case node of Dir dir -> (size dir <= 100000); File _ -> False) fs
  -- print $  sum $ map (size . fromDir) $ findNode (\node -> case node of Dir dir -> (size dir >= 100000); File _ -> False) fs

  return ()



