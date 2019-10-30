module Sort
    ( sort
    , msort
    , qsort
    , isort
    ) where

sort :: Ord a => [a] -> [a]
sort = msort

-- {{ Merge Sort }}

-- ASSUMPTION: xs and ys are sorted
merge :: Ord a => [a] -> [a] -> [a]
merge xs         []         = xs
merge []         ys         = ys
merge xs'@(x:xs) ys'@(y:ys)
    | x <= y    = x : merge xs  ys'
    | otherwise = y : merge xs' ys

halve :: [a] -> ([a],[a])
halve [ ]    = ([],[])
halve [x]    = ([x],[])
halve (x:x':xs) = (x:lxs, x':rxs)
    where
        (lxs,rxs) = halve xs

-- merge sort
msort :: Ord a => [a] -> [a]
msort [ ] = []
msort [z] = [z]
msort zs  = merge (msort xs) (msort ys)
    where
        (xs,ys)  = halve zs

-- {{ Quick Sort }}

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (w:xs) = qsort small ++ [w] ++ qsort large
    where
        small = [ x | x <- xs, x <= w ]
        large = [ x | x <- xs, x > w ]

-- {{ Insert Sort }}

-- ASSUMPTION FOR insert w xs:
--  xs is sorted

insert :: Ord a => a -> [a] -> [a]
insert w []     = [w]
insert w xs'@(x:xs)
    | w < x     = w : xs'
    | otherwise = x : insert w xs

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- {{ Is Sorted? }}

sorted :: Ord a => [a] -> Bool
sorted (x:x':xs) = x <= x' && sorted (x':xs)
sorted _         = True

sorted' :: Ord a => [a] -> Bool
sorted' []         = True
sorted' xs'@(x:xs) = and (zipWith (<=) xs xs')

-- {{ tests }}

prop_qsortLength xs = length xs == length (qsort xs)
prop_qsortSorts  xs = sorted (qsort xs)
prop_qsortQsort  xs = qsort xs  == qsort (qsort xs) 

-- prop_sortLength sort xs = length xs == length (sort xs)
-- prop_sortSorts  sort xs = sorted (sort xs)

-- prop_qsortLength' = prop_qsortLength
