module P3P1 where
  import Data.List.Split

  type Matrix = (Size, [Int])
  type Size = (Int, Int)

  initM :: Size -> Matrix
  initM s@(w, h) = (s, replicate (w * h) 0)

  intersectM :: Matrix -> Matrix -> Matrix
  intersectM (s', xs) (_, ys)= (s', zipWith (+) xs ys)

  data Claim = Claim {idC:: Int,  leC:: Int, teC:: Int, widthC :: Int, heightC :: Int} deriving (Show)

  -- The bounding box wrapping all claims
  boundingBoxC :: [Claim] -> Size
  boundingBoxC xs = (maximum $ map (\c -> teC c + heightC c) xs,
                      maximum $ map (\c -> leC c + widthC c) xs)

  -- convert linear list to matrix
  claimToMatrix :: Size -> Claim -> Matrix
  claimToMatrix (rows, columns) c = ((rows, columns), map go [(y', x') | x' <- [1..rows], y' <- [1..columns]])
    where
        go (x, y) = if withinBounds (x, y) c then 1 else 0
        -- Check if the x, y coordinate is within bounds.
        withinBounds :: (Int, Int) -> Claim -> Bool
        withinBounds (x, y) claim = x > le' && x <= lee' && y > te' && y <= tee'
              where (le', te', lee', tee') =
                      (leC claim, teC claim, le' + widthC claim, te' + heightC claim)

  -- #1 @ 126,902: 29x28
  fromString :: String -> Claim
  fromString ('#':xs) =
    let [id', _, start, size] = splitOn " " (filter (/=':') xs)
        [le', te'] = splitOn "," start
        [w', h'] = splitOn "x" size in
        Claim (read id') (read le') (read te') (read w') (read h')


  main :: IO ()
  main = do
    ls <- readFile "in.txt"
    let claims = map fromString $ lines ls
    let size' = boundingBoxC claims
    let matrix = foldr go (initM size') claims
            where go claim acc  = intersectM acc (claimToMatrix size' claim)
    print $ (length . filter (>1)) (snd matrix)
