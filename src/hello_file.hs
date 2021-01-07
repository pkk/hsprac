import System.IO

main :: IO ()
main = do 
    myFile <- openFile "./hello.txt" ReadMode
    firstLine <- hGetLine myFile
    putStrLn firstLine
    secondLine <- hGetLine myFile
    goodbyeFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyeFile secondLine
    hClose myFile
    hClose goodbyeFile
    putStrLn "done!"