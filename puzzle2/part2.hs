module P2P2 where

  distance :: String -> String -> Int
  distance xs ys = length xs - length (similar xs ys)

  similar :: String -> String -> String
  similar xs ys = filter (/=' ') $ zipWith go xs ys
      where go x y
              | x  == y = x
              | otherwise = ' '

  main :: IO ()
  main = do
      l <- readFile "in.txt"
      let ls = lines l
      print [similar first second  | first <- ls, second <- ls, distance first second == 1]
