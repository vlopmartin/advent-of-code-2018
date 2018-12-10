import Rect

main = do
    input <- readInput
    putStrLn $ transform input
 
readInput :: IO String
readInput = readFile "input"

transform :: String -> String
transform = show . getNotOverlapping . map toRect . lines

getNotOverlapping :: [Rect] -> [Rect]
getNotOverlapping xs = filter (\x -> null $ tail $ filter (overlaps x) xs) xs
