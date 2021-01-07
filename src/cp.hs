import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let orgFileName = args !! 0
    let newFileName = args !! 1
    fContent <- readFile orgFileName
    writeFile newFileName fContent