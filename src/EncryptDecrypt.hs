{-# LANGUAGE InstanceSigs #-}

module EncryptDecrypt where

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (not (v1 && v2))


xorPair :: (Bool, Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = 
    if (remainder == 0)
    then False : intToBits' nextVal
    else True : intToBits' nextVal
    where
        remainder = mod n 2
        nextVal = div n 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where
        reversedBits = reverse (intToBits' n)
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits = intToBits . fromEnum

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
    where
        size = length bits
        indices = [size-1, size-2 .. 0]
        trueLocations = filter(\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = 
    map (\pair -> (fst pair) `xor` (snd pair)) 
    (zip padBits plainTextBits)
    where
        padBits = map charToBits pad
        plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plaintext = 
    map bitsToChar bitList
    where
        bitList = applyOTP' pad plaintext