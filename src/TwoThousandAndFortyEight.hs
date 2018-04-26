module TwoThousandAndFortyEight
    ( runProgram
    , drawGrid
    ) where

data Direction = Left' | Right'

line :: (Num t, Show t) => [t] -> [String]
line xs = map show xs

-- main :: IO()
-- main = runProgram drawGrid

runProgram :: (Num t, Show t) => [[t]] -> IO()
runProgram grid = do
  putStrLn $ concat (map drawLine $ map line grid)
  putStr "Do one movement (h, j, k, l): "
  name <- getLine
  putStrLn $ "Your movement was: " ++ name
  runProgram $ map moveAndSquash drawGrid


-- @TODO: after grab the movement, should we clean the scream and draw the new board?

drawLine :: [String] -> String
drawLine (a:b:c:d:_) = " [ " ++ show a ++ " ] [ " ++ show b ++ " ] [ " ++ show c ++ " ] [ " ++ show d ++ " ]\n"

drawGrid :: (Num t) => [[t]]
drawGrid = [
    [0, 0, 0, 0],
    [4, 0, 4, 0],
    [0, 2, 0, 0],
    [8, 0, 0, 8] ]

-- ask user to do one movement (h,j,k,l)

moveAndSquash :: (Num a, Eq a) => [a] -> [a]
moveAndSquash = squashToTheRight . moveRight

move :: (Num a, Eq a) => Direction ->  [a] -> [a]
move Right' y = moveRight y
move Left' ys = reverse $ moveRight $ reverse ys

-- Move elements in the row to the right side
-- We don't consider zero as proper number
-- TODO: use Maybe / Just
moveRight :: (Num a, Eq a) => [a] -> [a]
moveRight xs = squashToTheRight r
    where
        nz = filter (\x -> x /= 0) xs
        pad = replicate (length xs - length nz) 0
        r = pad ++ nz

squashToTheRight :: (Num a, Eq a) => [a] -> [a]
squashToTheRight (x:y:xs)
    | x == y    = (0 : x + y : squashToTheRight xs)
    | otherwise = (x: squashToTheRight (y:xs))
squashToTheRight (x:[]) = [x]
squashToTheRight _ = []
