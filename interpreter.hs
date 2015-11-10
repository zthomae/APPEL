-- I don't know Haskell, and I don't know APL.
-- Here I attempt to learn both.

-- Define binary math operators on lists
opI :: (a -> a -> a) -> [a] -> [a] -> [a]
opI f (x:[]) (y:[]) = [f x y]
opI f (x:xs) (y:[]) = (f x y : opI f xs [y])
opI f (x:[]) (y:ys) = (f x y : opI f [x] ys)
opI f (x:xs) (y:ys)
    | length xs == length ys = (f x y : opI f xs ys)
    | otherwise = error "Mismatched lengths"

addI = opI (+)
subI = opI (-)
timesI = opI (*)
divI = opI div

-- reshape: turn a 1-dimensional list into an x by y table
-- This is not general enough -- consider rank 3 arrays
reshape :: Int -> Int -> [a] -> [[a]]
reshape x y t
    | x == 1 && length t == y = [take y t]
    | x > 1 && length t > y = [take y t] ++ reshape (x-1) y (drop y t)
    | otherwise = error "Invalid dimensions"

-- dimT: Get the dimensions of a table
-- This assumes that the table is well-formed...
dimT :: [[a]] -> (Int, Int)
dimT (row:rows) = (length rows + 1, length row)

-- join: Combine two tables with the same number of rows
join :: [[a]] -> [[a]] -> [[a]]
join (x:[]) (y:[]) = [x ++ y]
join (x:xs) (y:ys)
    | length xs == length ys = [x ++ y] ++ join xs ys
    | otherwise = error "Mismatched number of rows"
