getPerSquareInchCost :: Double -> Double -> Double
getPerSquareInchCost size cost = cost / (3.14 * (size / 2 ) * (size / 2))

printCheaperPizza :: String -> Double -> String
printCheaperPizza size cost = "The " ++ show size ++ " pizza is cheaper at" ++
                        show cost ++ "per square inch"

main = 
    do
        putStrLn "What is the size of pizza 1"
        pizza1Size <- getLine
        putStrLn "What is the cost of pizza 1"
        pizza1Cost <- getLine
        putStrLn "What is the size of pizza 2"
        pizza2Size <- getLine
        putStrLn "What is the cost of pizza 2"
        pizza2Cost <- getLine
        let pizza1PerInchCost = getPerSquareInchCost (read pizza1Size) (read pizza1Cost)
        let pizza2PerInchCost = getPerSquareInchCost (read pizza2Size) (read pizza2Cost)
        if (pizza1PerInchCost < pizza2PerInchCost)
        then putStrLn (printCheaperPizza pizza1Size pizza1PerInchCost)
        else putStrLn (printCheaperPizza pizza2Size pizza2PerInchCost)
        