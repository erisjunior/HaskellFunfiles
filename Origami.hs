module Origami where

import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , scanl, scanr
    , sum , product
    , length
    , concat
    , filter
    , map
    , any , all
    , and , or
    , takeWhile , dropWhile
    )

import qualified Prelude as P

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> b -> b) -> [a] -> b
foldr f []     = error "lista vazia"
foldr f (x:xs) = f x (foldr f xs)

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b -> a -> b) -> [a] -> b
foldl _ []     = error "lista vazia"
foldl f (x:xs) = f (foldl f xs) x

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a 
foldr1 _ []     = error "lista vazia"
foldr1 f [x,x'] = f x x'
foldr1 f (x:xs) = f x (foldr1 f xs)

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a 
foldl1 _ []     = error "lista vazia"
foldl1 f [x,x'] = f x x'
foldl1 f (x:xs) = f (foldl1 f xs) x



--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ _ []     = error "lista vazia"
scanl f y [x]    = [f y x]
scanl f y (x:xs) = f y x : scanl f (f y x) xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ _ []     = error "lista vazia"
scanr f y [x]    = [f y x]
scanr f y (x:xs) = foldr f (x:xs) : scanl f y xs


--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum [] = 0
sum xs = foldr1 (+) xs

product :: Num a => [a] -> a
product [] = 1
product xs = foldl1 (*) xs

concat :: [[a]] -> [a]
concat [] = []
concat xs = foldr1 (++) xs

any :: (a -> Bool) -> [a] -> Bool
any f [] = True
any f xs = foldr1 oneTrue xs

oneTrue :: Bool -> Bool -> Bool
oneTrue False False = False
oneTrue _     _     = True

oneFalse :: Bool -> Bool -> Bool
oneFalse = not . oneTrue

twoFalses :: Bool -> Bool -> Bool
twoFalses False False = False
twoFalses _     _     = True

twoTrues :: Bool -> Bool -> Bool
twoTrues = not . twoFalses

all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f xs = foldr1 twoTrues xs

and :: [Bool] -> Bool
and [] = True
and xs = foldr1 twoTrues xs

or :: [Bool] -> Bool
or [] = True
or xs = foldr oneTrue xs

length :: Integral i => [a] -> i
length = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

reverse :: [a] -> [a]
reverse = undefined

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = undefined

-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: Integral i => [i] -> (i, Maybe i)
semo = undefined

-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: Eq a => [a] -> [a]
remdups = undefined

safeLast :: [a] -> Maybe a
safeLast = undefined

-- dec2int [1,9,9,2] = 1992
dec2int :: Integral i => [i] -> i
dec2int = undefined

-- drop m . drop n  = undefined
-- map g . map f  = undefined
