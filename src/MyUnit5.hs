import qualified Data.Map as Map

c1 :: Maybe Int -> Maybe String
c1 (Just x) = Just (show x)
c1 _ = Nothing

intToStr :: Int -> String
intToStr x = show x

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just x) = Just (reverse x)
reverseMaybe _ = Nothing

-- instance Functor Maybe where
--    fmap func (Just n) = Just (func n)
--    fmap func Nothing = Nothing

data Box a = Box a deriving Show


instance Functor Box where
    fmap func (Box a) = Box (func a)

morePresents :: Box a -> Int -> Box [a]
morePresents (Box a) n = fmap (take n . repeat) (Box a)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x
