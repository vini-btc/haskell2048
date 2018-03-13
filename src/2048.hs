module TwoThousandAndFortyEight where

line :: [Int] -> [String]
line xs = map show xs

main :: IO()
main = do
  putStrLn $ concat x
    where x = map drawLine $ map line drawGrid

drawLine :: [String] -> String
drawLine (a:b:c:d:xs) = " [ " ++ show a ++ " ] [ " ++ show b ++ " ] [ " ++ show c ++ " ] [ " ++ show d ++ " ]\n"

drawGrid :: (Num t) => [[t]]
drawGrid = [
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0] ]

-- Move elements in the row to the right side
-- We don't consider zero as proper number
-- TODO: use Maybe / Just
moveRight :: (Num a, Eq a) => [a] -> [a]
moveRight xs = r
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

