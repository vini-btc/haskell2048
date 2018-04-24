module TwoThousandAndFortyEight where

-- type Grid = []
type Line = [Int]

-- grid = map (\x -> Just x :: Cell) [2, 4, 4, 8]
grid = [2, 4, 4, 8]

line :: [Int] -> [String]
line xs = map show xs 

moveRight :: (Num a, Eq a) => [a] -> [a]
moveRight (0:0:xs) = (0:0:moveRight xs)
moveRight (0:x:xs) = 0 : moveRight (x:xs)
moveRight (x:0:xs) = 0 : moveRight (x:xs)
moveRight (x:y:[0]) = (0: moveRight (x:[y]))
moveRight (x:xs) = (x: moveRight xs)
moveRight xs = xs

-- sumRight

toRight :: (Num a, Eq a) => [a] -> [a] 
toRight (x:y:xs)
    | x == y    = (0 : x + y : toRight xs) 
    | otherwise = (x: toRight (y:xs))
toRight (x:[]) = [x]


-- toRight [2,2,2,2] == [0,0,4,4]
-- toRight [2,0,2,0] == [0,0,2,2]
