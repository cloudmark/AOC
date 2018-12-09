module P2P1 where

  import Data.List
  import Control.Arrow

  checksum :: Int -> String -> Int
  checksum x = (\y -> if y then 1 else 0) . (x `elem`) . map length . group . sort

  checksum2 :: [String] -> Int
  checksum2 = sum . map (checksum 2)

  checksum3 :: [String] -> Int
  checksum3 = sum . map (checksum 3)

  main :: IO ()
  main = do
      l <- readFile "in.txt"
      let ls = lines l
      print $ uncurry (*) $ (checksum2 &&& checksum3) ls
