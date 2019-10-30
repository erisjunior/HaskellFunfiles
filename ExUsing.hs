module ExUsing where

import Prelude hiding
    ( filter
    )

type Pred a = (a -> Bool)

-- using concat
filter :: Pred a -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = concat [y, filter p xs]
    where y = if p x then [x] else [] 

-- using zipWith
sorted :: Ord a => [a] -> Bool
sorted []         = True
sorted xs'@(x:xs) = and (zipWith (<=) xs xs') 

-- using zipWith
fibs :: Integral i => [i]
fibs = [0, 1] ++ zipWith (+) (map fib [1..]) (map fib [0..])

fib :: Integral i => i -> i
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
