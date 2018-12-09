module P1P2 where

  dupsFolder :: [Int] -> Int
  dupsFolder =  go (0, [0]) .  cycle
      where go (sum', buff) (x:xs)
              | x' `elem` buff = x'
              | otherwise = go (x', x' : buff) xs
                where x' = x + sum'

  main :: IO ()
  main = do
      l <- readFile "a.txt"
      print $ (dupsFolder . map (read . filter (/='+')) . lines) l
