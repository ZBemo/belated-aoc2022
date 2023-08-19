module Utils where

removeCarriage = filter (/= '\r')

split :: Char -> String -> [String]
split c = wordsWhen (== c)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> (w:) $! wordsWhen p s''
    where
      (w, s'') = break p s'
