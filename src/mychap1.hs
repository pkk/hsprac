simple x = x
a = 3
inc = (+) 1
double = (*) 2
square n = n * n

ifEven n = if (even n) then n - 2 else 3*n + 1

ifEvenInc n = ifEven $ inc $ n

genIfEven f = (\x -> ifEven $ f $ x)

makeAdder :: Int -> (Int -> Int)
makeAdder n = (\x -> x + n)

addXto3 :: Int -> Int
addXto3 = makeAdder 3

myAdd :: Int -> Int -> Int
myAdd a b = a + b

flipTwoArgs twoArgFunction = (\x y -> twoArgFunction y x)

binaryPartialApplication binaryFunction anArg 
    = (\x -> binaryFunction anArg x)

myReverse :: [a] -> [a]
myReverse = go []
    where
        go :: [a] -> [a] -> [a]
        go ys [] = ys
        go ys (x:xs) = go (x:ys) xs

myRepeat :: a -> [a]
myRepeat a = a : myRepeat a

subseq :: Int -> Int -> [a] -> [a]
subseq s e list = take (e - s) (drop s list)

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n - 1) xs

mygcd :: Int -> Int -> Int
mygcd a b = if rem == 0 
            then b
            else mygcd b rem
            where
                rem = mod a b

sayAmount :: Int -> String
sayAmount n = case n of 
    1 -> "One"
    2 -> "two"
    n -> "a bunch"


myLen :: [a] -> Int
myLen = go 0
    where 
        go :: Int -> [a] -> Int
        go n [] = n
        go n (_:xs) = go (n+1) xs

myCycle :: [a] -> [a]
myCycle xs = xss where xss = xs ++ xss

ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))


frev :: [a] -> [a]
frev = foldl (\acc ele -> (ele : acc)) []

harmonic n = sum (take n svalues)
    where spairs = zip (cycle [1.0]) [1.0,2.0 .. ]
          svalues = map (\(x,y) -> x / y) spairs