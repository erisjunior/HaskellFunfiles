module ExFromWhile where

fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
fromWhile n p = takeWhile p . drop n

fromFor :: Int -> Int -> [a] -> [a]
fromFor n m = take m . drop n

fromTo :: Int -> Int -> [a] -> [a]
--fromTo n m = take (m+n) . drop n
fromTo n m = fromFor n (m - n + 1)

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
--fromToThat n m p = fromWhile n p . drop (m+n)
fromToThat n m p = filter p . fromTo n m
