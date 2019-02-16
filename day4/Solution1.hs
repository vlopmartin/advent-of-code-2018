import Events
import Shifts
import Data.List (sort, sortBy)
import Data.Ord (Down(..))
import Data.Functor.Contravariant ((>$<), getComparison, defaultComparison)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

main = do
    input <- readInput
    putStrLn $ transform input
 
readInput :: IO String
readInput = readFile "input"

transform :: String -> String
transform s = show $ findMinute guardId shifts * guardId
  where
    shifts = eventsToShifts $ sort $ map toEvent $ lines s
    guardId = findLargest $ countHours $ shifts

countHours :: [Shift] -> IntMap Int
countHours = foldr addShift IntMap.empty 
  where
    addShift :: Shift -> IntMap Int -> IntMap Int
    addShift (key, xs) = IntMap.insertWith (+) key (count id xs)

findLargest :: Ord a => IntMap a -> IntMap.Key
findLargest = fst . head . sortBy largestSnd . IntMap.toList
  where
    largestSnd = getComparison $ Down . snd >$< defaultComparison

findMinute :: Id -> [Shift] -> Int
findMinute key = findLargest . foldr accumulate IntMap.empty . filter ((== key) . fst)
  where
    accumulate shift = IntMap.unionWith (+) $ fromIndexList $ minutes shift
    fromIndexList = IntMap.fromList . zip [0..]
    minutes = map (\x -> if x then 1 else 0) . snd

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

