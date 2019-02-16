module Polymer where

import Data.Char

type Polymer = String

-- Holy moly this is fast for a large list
react :: Polymer -> Polymer
react = foldr accumulate []
  where
    accumulate x [] = [x]
    accumulate x xs
      | x `areOpposite` head xs = tail xs
      | otherwise               = x:xs

areOpposite :: Char -> Char -> Bool
areOpposite x y
  | x == y    = False
  | isUpper x = toLower x == y
  | otherwise = toUpper x == y

