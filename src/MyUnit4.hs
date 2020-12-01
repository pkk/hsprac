module MyUnit4 where

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let stmt = helloPerson name
    putStrLn stmt