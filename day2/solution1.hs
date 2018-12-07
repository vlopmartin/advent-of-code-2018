import Data.List (sort, group)

main = do
    input <- readInput
    putStrLn $ transform input
 
readInput :: IO String
readInput = readFile "input"

transform :: String -> String
transform = show . checksum . lines

checksum :: [String] -> Int
checksum xs = (count fst $ pairs xs) * (count snd $ pairs xs)
    where pairs = map (counts . group . sort)

counts :: [[a]] -> (Bool, Bool)
counts xs = (hasN 2 xs, hasN 3 xs)

hasN :: Int -> [[a]] -> Bool
hasN n = any (\xs -> length xs == n)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
