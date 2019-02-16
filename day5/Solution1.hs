import Polymer

main = do
    input <- readInput
    putStrLn $ transform input

readInput :: IO String
readInput = readFile "input"

transform :: String -> String
transform = show . length . react . init

