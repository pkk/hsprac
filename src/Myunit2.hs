{-# LANGUAGE InstanceSigs #-}

module Myunit2 where

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) = if f x then (x: rest) else rest
    where rest = myFilter f xs

newtype Diameter = Diameter Int deriving (Show)

a = Diameter 10

class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe :: Bool -> String
    describe True = "A member of Bool class, True is oppostive of False"
    describe _ = "A member of Bool class, False is oppostive of True"

data Avengers = THOR | VISION | HULK | IRONMAN 

instance Eq Avengers where
    (==) :: Avengers -> Avengers -> Bool
    (==) THOR THOR = True
    (==) VISION VISION = True
    (==) HULK HULK = True
    (==) IRONMAN IRONMAN = True
    (==) _ _ = False

instance Ord Avengers where
    compare :: Avengers -> Avengers -> Ordering
    compare THOR _ = GT
    compare VISION THOR = LT
    compare VISION _ = GT
    compare HULK IRONMAN = GT
    compare HULK _ = LT
    compare IRONMAN _ = LT

instance Bounded Avengers where
    maxBound :: Avengers
    maxBound = IRONMAN
    minBound :: Avengers
    minBound = THOR

instance Enum Avengers where
    toEnum :: Int -> Avengers
    toEnum 0 = THOR
    toEnum 1 = VISION
    toEnum 2 = HULK
    toEnum 3 = IRONMAN
    fromEnum :: Avengers -> Int
    fromEnum THOR = 0
    fromEnum VISION = 1
    fromEnum HULK = 2
    fromEnum IRONMAN = 3

instance Show Avengers where
    show :: Avengers -> String
    show THOR = "Norse God Thor"
    show VISION = "Mind Stone Vision"
    show HULK = "Mighty Hulk"
    show IRONMAN = "Tony IronMan Stark"

cycleAvengers :: Avengers -> Avengers
cycleAvengers avenger
    | avenger == maxBound = minBound 
    | otherwise = succ avenger


