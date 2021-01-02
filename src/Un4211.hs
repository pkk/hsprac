import Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

personNames :: Map.Map Int String
personNames = Map.fromList [(0,"Krishna")]

mayBeMain :: Maybe String
mayBeMain = do
    name <- Map.lookup 0 personNames
    let stmt = helloPerson name
    return stmt

getContext :: Maybe String -> String
getContext (Just s) = s
getContext _ = "Nothing"

main :: IO ()
main = do
    let stm = mayBeMain
    putStrLn (getContext stm)