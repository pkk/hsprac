import qualified Data.Map as Map
import Data.List

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids:: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawersContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawersContents ids catalog = map (\id -> Map.lookup id catalog) ids

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawersContents possibleDrawers organCatalog

countOrgans :: Organ -> [Maybe Organ] -> Int
countOrgans organ available = length (filter (\x -> x == Just organ) availableOrgans)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething _ = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just x) = show x
showOrgan _ = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ". " organList