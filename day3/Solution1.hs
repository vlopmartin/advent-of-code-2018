import Rect

main = do
    input <- readInput
    putStrLn $ transform input
 
readInput :: IO String
readInput = readFile "input"

-- Kind of slow but I don't think there's anything I can do about this one?
transform :: String -> String
transform = show . onAllPoints . map toRect . lines
    where onAllPoints xs = count (isInTwoOrMore xs) allPoints

isInTwoOrMore :: [Rect] -> Point -> Bool
isInTwoOrMore xs p = (count (\x -> isIn x p) xs) >= 2

allPoints :: [Point]
allPoints = [Point x y | x <- [1..1000], y <- [1..1000]]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
