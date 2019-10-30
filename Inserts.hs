module Inserts where

inserts :: a -> [a] -> [[a]]
inserts c []     = [[c]]
inserts c (x:xs) = (c:x:xs) : map (x:) (inserts c xs)

inserts' :: a -> [a] -> [[a]]
inserts' c xs =
    [ insertAt c i xs | i <- [0 .. length xs ] ]

insertAt :: a -> Int -> [a] -> [a]
insertAt y 0 xs = y : xs
insertAt y n [] = [y]
insertAt y n xs = before ++ [y] ++ after
    where
        (before, after) = splitAt n xs

cross :: ((a -> b), (c -> d)) -> (a, c) -> (b, d)
cross (f,g) (x, y) = (f x, g y)

pair :: ((a -> b), (a -> c)) -> a -> (b,c)
pair (f,g) x = (f x, g x)

dup :: a -> (a,a)
dup = pair (id,id)

f :: Int -> Int
f = uncurry (+) . dup . uncurry (*) . dup
