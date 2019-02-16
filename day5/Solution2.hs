import Polymer
import Data.Char

main = do
    input <- readInput
    putStrLn $ transform input

readInput :: IO String
readInput = readFile "input"

transform :: String -> String
transform = show . minLength . init
  where
    minLength = minimum . map (uncurry lengthIfRemoved) . pairs
    pairs xs = map (flip (,) $ xs) ['A'..'Z']

-- There has to be an easier way to "and" two predicates, right?
lengthIfRemoved :: Char -> Polymer -> Int
lengthIfRemoved x = length . react . filter (\y -> y /= x && y /= toLower x)

