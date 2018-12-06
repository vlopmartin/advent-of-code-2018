main = do
    input <- readInput
    putStrLn $ transform input
           
readInput :: IO String
readInput = readFile "input"

transform :: String -> String
transform = show . sum . numbers

numbers :: String -> [Int]
numbers = map (read . exclude '+') . lines
    where exclude chr = foldr (appendUnless chr) []
          appendUnless chr x xs = if x == chr then xs else x:xs

