import Events
import Shifts
import Data.List (sort, maximumBy, elemIndex)
import Data.Ord (Down(..))
import Data.Functor.Contravariant ((>$<), getComparison, defaultComparison)
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap

main = do
    input <- readInput
    putStrLn $ transform input
 
readInput :: IO String
readInput = readFile "input"

transform :: String -> String
transform s = show $ findMinute guardId minutes * guardId
  where
    shifts = eventsToShifts $ sort $ map toEvent $ lines s
    minutes = minuteCounts shifts
    guardId = findGuard minutes
    findMinute key = maxIndex . (! key)

minuteCounts :: [Shift] -> IntMap [Int]
minuteCounts = foldr addShift IntMap.empty
  where
    addShift :: Shift -> IntMap [Int] -> IntMap [Int]
    addShift (key, xs) = IntMap.insertWith (zipWith (+)) key (map (\x -> if x then 1 else 0) xs)

findGuard :: IntMap [Int] -> IntMap.Key
findGuard = fst . maximumBy largestMax . IntMap.toList
  where
    largestMax = getComparison $ maximum . snd >$< defaultComparison

maxIndex :: Ord a => [a] -> Int
maxIndex xs = maybe undefined id $ elemIndex (maximum xs) xs

