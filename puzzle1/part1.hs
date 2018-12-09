module P1P1 where

  main :: IO ()
  main = do
      l <- readFile "a.txt"
      print $ (sum . map (read . filter (/='+')) . lines) l
