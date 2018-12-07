import Data.List (tails)

main = do
    input <- readInput
    putStrLn $ transform input
 
readInput :: IO String
readInput = readFile "input"

transform :: String -> String
transform = show . sameLetters . findPair . lines

findPair :: [String] -> (String, String)
findPair = head . filter (\x -> differBy x == 1) . pairs

pairs :: [a] -> [(a, a)]
pairs = concat . map pairsWithHead . tails

pairsWithHead :: [a] -> [(a, a)]
pairsWithHead (x:xs) = (,) x <$> xs

differBy :: Eq a => ([a], [a]) -> Int
differBy (xs, ys) = count not $ zipWith (==) xs ys

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

sameLetters :: (String, String) -> String
sameLetters (xs, ys) = concatMaybe $ zipWith maybeEq xs ys
    where maybeEq a b = if a == b then Just a else Nothing
          concatMaybe = foldr appendMaybe []
          appendMaybe (Just x) = (:) x
          appendMaybe Nothing = id
