module Sort
    ( sort
    , msort
    , qsort
    , isort
    ) where

sort :: Ord a => [a] -> [a]
sort = msort

-- ASSUMPTION: xs and ys are sorted
merge :: Ord a => [a] -> [a] -> [a]
merge xs         []         = xs
merge []         ys         = ys
merge xs'@(x:xs) ys'@(y:ys)
    | x <= y    = x : merge xs  ys'
    | otherwise = y : merge xs' ys

--halve :: [a] -> ([a],[a])
--halve [ ]    = ([],[])
--halve [x]    = ([x],[])
--halve (x:xs) = (x:xs', xs'')
--  where
--    (xs', xs'') = halve xs

-- merge sort
msort :: Ord a => [a] -> [a]
msort [ ] = []
msort [z] = [z]
msort zs  = merge (msort xs) (msort ys)
    where
        (xs,ys)  = splitAt midpoint zs
        midpoint = length  zs `div` 2

qsort :: Ord a => [a] -> [a]
qsort = msort

isort :: Ord a => [a] -> [a]
isort = msort
