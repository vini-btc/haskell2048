module TwoThousandAndFortyEight where

line :: [Int] -> [String]
line xs = map show xs 

-- Move elements in the row to the right side
-- We don't consider zero as proper number
-- TODO: use Maybe / Just
moveRight :: (Num a, Eq a) => [a] -> [a]
moveRight xs = r
    where
        nz = filter (\x -> x /= 0) xs
        pad = replicate (length xs - length nz) 0
        r = pad ++ nz

toRight :: (Num a, Eq a) => [a] -> [a] 
toRight (x:y:xs)
    | x == y    = (0 : x + y : toRight xs) 
    | otherwise = (x: toRight (y:xs))
toRight (x:[]) = [x]

