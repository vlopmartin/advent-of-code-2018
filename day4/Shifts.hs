module Shifts where

import Events
import Data.List (foldl')

type Shift = (Id, [Bool])

add :: Event -> Shift -> Shift
add (Event time Asleep) (id, xs) = (id, substituteFrom (minutes time) True xs)
add (Event time Awake) (id, xs) = (id, substituteFrom (minutes time) False xs)

substituteFrom :: Int -> a -> [a] -> [a]
substituteFrom i x xs = take i xs ++ replicate (60-i) x

empty :: Id -> Shift
empty id = (id, replicate 60 False)

-- Assumes events are sorted
eventsToShifts :: [Event] -> [Shift]
eventsToShifts = extract . foldl' accumulate (Nothing, [])
  where
    accumulate (Nothing, xs) (Event _ (Guard id)) = (Just $ empty id, xs)
    accumulate (Just shift, xs) (Event _ (Guard id)) = (Just $ empty id, xs++[shift])
    accumulate (Just shift, xs) event = (Just $ add event shift, xs) 
    extract (Just shift, xs) = shift:xs

