import System.Environment
import Data.List

main :: IO ()
main = do 
    args <- getArgs
    let fileName = head args
    fileContent <- readFile fileName
    let (cc, wc, lc) = getCounts fileContent
    putStrLn $ getOutputText fileName (getCounts fileContent)

getCounts :: String -> (Int, Int, Int)
getCounts input = (cCount, wCount, lCount)
    where cCount = length input
          wCount = length $ words $ input
          lCount = length $ lines $ input
        
getOutputText :: String -> (Int, Int, Int) -> String
getOutputText fName (cc,wc,lc) = 
    fName ++ " chars: "  ++ show cc ++ " words: " ++ show wc ++ " lines: " ++ show lc