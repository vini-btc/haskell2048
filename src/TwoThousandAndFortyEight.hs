module TwoThousandAndFortyEight
    ( runProgram
    , drawGrid
    , move
    , Direction(Right', Left', Up')
    ) where

import Data.List
import Data.Function

-- Should we have a NONE position? *Monad*?
data Direction = Left' | Right' | Up' | Down'

line :: (Num t, Show t) => [t] -> [String]
line xs = map show xs

runProgram :: (Num t, Show t) => [t] -> IO()
runProgram grid = do
  putStrLn $ concat (map drawLine $ map line $ getRows grid)
  putStr "Do one movement (h, j, k, l): "
  input <- getLine
  putStrLn $ "\nYour movement was: " ++ input
  runProgram $ move (resolveInput input) drawGrid

resolveInput :: String -> Direction
resolveInput "h" = Left'
resolveInput "l" = Right'
resolveInput "k" = Up'
resolveInput  _  = Right'

-- @TODO: after grab the movement, should we clean the scream and draw the new board?

drawLine :: [String] -> String
drawLine (a:b:c:d:_) = " [ " ++ show a ++ " ] [ " ++ show b ++ " ] [ " ++ show c ++ " ] [ " ++ show d ++ " ]\n"

drawGrid :: (Num t) => [t]
drawGrid = [
    0, 0, 0, 0,
    4, 0, 4, 0,
    0, 2, 0, 0,
    8, 0, 0, 8 ]

-- ask user to do one movement (h,j,k,l)

getColumns :: (Num a) => [[a]] -> [[a]]
getColumns [] = []
getColumns xs = filterNotEmpty $ [map head xs'] ++ getColumns (map tail xs')
  where
    xs' = filterNotEmpty xs
    filterNotEmpty = filter (not . null)

getRows :: (Num a) => [a] -> [[a]]
getRows [] = []
getRows (v:x:y:z:xs) = [v, x, y, z]:[] ++ getRows xs

move :: (Num a, Eq a) => Direction -> [a] -> [a]
move Right' xs = concat $ map mergeRow $ getRows xs
move Left' xs = reverse $ move Right' $ reverse xs
move Up' xs = concat $ getColumns $ map reverse $ getRows $ move Right' (concat $ getColumns $ getRows xs)

-- TODO: use Maybe / Just
mergeRow :: (Num a, Eq a) => [a] -> [a]
mergeRow xs = padLeft originalSize $ squashToTheRight r
    where
        originalSize = length xs
        nz = filter (\x -> x /= 0) xs
        r = padLeft originalSize nz

padLeft :: (Num a, Eq a) => Int -> [a] -> [a]
padLeft n xs =
    let
        numberOfZeros = n - (length xs)
    in
    (replicate numberOfZeros 0) ++ xs


squashToTheRight :: (Num a, Eq a) => [a] -> [a]
squashToTheRight (x:y:xs)
    | x == y    = (x + y :  squashToTheRight xs)
    | otherwise = (x: squashToTheRight (y:xs))
squashToTheRight (x:[]) = [x]
squashToTheRight _ = []
