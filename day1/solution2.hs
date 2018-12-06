main = do
    input <- readInput
    putStrLn $ transform input
           
readInput :: IO String
readInput = readFile "input"

transform :: String -> String
transform = show . findDuplicate . sums . cycle . numbers

numbers :: String -> [Int]
numbers = map (read . exclude '+') . lines
    where exclude chr = foldr (appendUnless chr) []
          appendUnless chr x xs = if x == chr
                                  then xs
                                  else x:xs

-- Note to self: scanl works on infinite lists, foldl does not
sums :: Num a => [a] -> [a]
sums = tail . scanl (+) 0

-- WHY is this one so slow even when I'm doing the recursion myself
findDuplicate :: Eq a => [a] -> a
findDuplicate = findDuplicateOn []
    where findDuplicateOn xs (y:ys) = if y `elem` xs
                                      then y
                                      else findDuplicateOn (y:xs) ys
                      
