-- I don't know Haskell, and I don't know APL.
-- Here I attempt to learn both.

-- Define binary math operators on lists
opI :: (a -> a -> a) -> [a] -> [a] -> [a]
opI f xs (y:[]) = map (\x -> f x y) xs
opI f (x:[]) ys = map (\y -> f x y) ys
opI f x y =
    let opI' f (x:xs) (y:ys) l = opI' f xs ys (l ++ [f x y]) 
        opI' f [] [] l = l
        opI' f _ _ _ = error "Mismatched list lengths"
    in  opI' f x y []

addI = opI (+)
subI = opI (-)
timesI = opI (*)
divI = opI div

-- reshape: turn a 1-dimensional list into an x by y table
-- This is not general enough -- consider rank 3 arrays
reshape :: Int -> Int -> [a] -> [[a]]
reshape 0 _ _ = []
reshape x y t =
    let reshape' ct 0 l = l
        reshape' ct x l = reshape' (drop y ct) (x-1) (l ++ [take y ct])
    in  case (signum(x), signum(y)) of
             (1, 1) -> reshape' (cycle t) x []
             (_, _) -> error "Invalid dimensions"

-- dimT: Get the dimensions of a table
-- This assumes that the table is well-formed...
dimT :: [[a]] -> (Int, Int)
dimT (row:rows) = (length rows + 1, length row)

-- join: Combine two tables with the same number of rows
join :: [[a]] -> [[a]] -> [[a]]
join (x:[]) (y:[]) = [x ++ y]
join (x:xs) (y:ys) = [x ++ y] ++ join xs ys
join _ _ = error "Mismatched number of rows"
