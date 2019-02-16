module Events where

import Data.Time
import Data.Bifunctor (bimap)

type Id = Int

data EventType = Guard Id | Asleep | Awake deriving (Show, Eq)
data Event = Event LocalTime EventType deriving (Show, Eq)

toEventType :: String -> EventType
toEventType "falls asleep" = Asleep
toEventType "wakes up" = Awake
toEventType s = Guard $ read $ tail $ find (startsWith '#') $ words s
    where startsWith c = (== c) . head

toEvent :: String -> Event
toEvent = uncurry Event . bimap (toTime . tail) (toEventType . tail . tail) . break (== ']')
    where toTime = parseTimeOrError True defaultTimeLocale "%F %R"

instance Ord Event where
    Event d1 _ `compare` Event d2 _ = d1 `compare` d2

find :: (a -> Bool) -> [a] -> a
find f = head . filter f

minutes = todMin . localTimeOfDay
