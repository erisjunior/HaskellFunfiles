module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

data Rat = Rat Integer Integer

instance Show Rat where
    show = undefined 

instance Eq Rat where
    Rat x y == Rat n m = x * m == y * n

instance Num Rat where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Ord Rat where
    compare = undefined

rat :: Integer -> Integer -> Rat
rat x y
    | y == 0    = error "Burro"
    | otherwise = Rat x y

(//) :: Rat -> Rat -> Rat
(//) = undefined

denominator :: Rat -> Integer
denominator _ m = m

numerator :: Rat -> Integer
numerator   n _ = n

