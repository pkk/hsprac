module MyUnit4 where

import System.Random

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let stmt = helloPerson name
    putStrLn stmt
    let mmin = read "1" :: Int
    let mmax = read "6" :: Int
    dieRoll <- randomRIO (minDie, maxDie)
    putStrLn (show dieRoll)