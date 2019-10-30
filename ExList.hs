module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

head :: [a] -> a
head []     = error "Empty List"
head (x:_) = x 

tail :: [a] -> [a]
tail []     = error "Empty List"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []     = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = snoc x xs'
    where xs' = reverse xs

(++) :: [a] -> [a] -> [a]
(++) x      []      = x
(++) []     x       = x
(++) (x:xs) (y:ys)  = (x:(xs ++ (y:ys)))

infixr 5 ++

snoc :: a -> [a] -> [a]
snoc x []     = [x]
snoc x (y:ys) = y : snoc x ys

flip :: ( a -> b -> c ) -> (b -> a -> c)
flip f x y = f y x

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

infixl 5 +++

minimum :: Ord a => [a] -> a
minimum []     = error ""
minimum [x]    = x
minimum (x:xs)
    | x < min = x
    | otherwise  = min
        where 
           min = minimum xs

maximum :: Ord a => [a] -> a
maximum []     = error ""
maximum [x]    = x
maximum (x:xs) = let m = maximum xs
                 in if x < m then x else m

take :: Integral i => i -> [a] -> [a]
take 0 _      = []
take _ []     = []
take n (x:xs) = x : take (n-1) xs

drop :: Integral i => i -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile f (x:xs) = if(f x) then (x: takeWhile f xs ) else ([])

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile f (x:xs) = (if(f x) then (xs) else (dropWhile f xs))

tails :: [a] -> [[a]]
tails []        = [[]]
tails xs'@(_:xs) = xs' : tails xs

init ::  [a] -> [a]
init [x]     = []
init (x:xs)  = x : init xs

inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

combinations :: [a] -> [[a]]
combinations []     = [[]]
combinations xs     = [xs] ++ combinations (init xs)

subsequences :: [a] -> [[a]]
subsequences []     = []
subsequences (x:xs) = map (x:) (subsequences xs) ++ subsequences xs

any :: (a -> Bool) -> [a] -> Bool
any _ []     = True
any f [x]    = f x
any f (x:xs) = if(f x) then True else (any f xs)

all :: (a -> Bool) -> [a] -> Bool
all _ []     = True
all f [x]    = f x
all f (x:xs) = if(f x) then (all f xs) else False

and :: [Bool] -> Bool
and []        = True
and [x]       = x
and (x:xs)    = if (x) then and xs else False

or  :: [Bool] -> Bool
or  []        = True
or  [x]       = x
or  (x:xs)    = if (x) then True else or xs

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

elem  :: (Eq a) => a -> [a] -> Bool
elem  _   []  = False
elem  y   xs  = any (==y) xs

elem'  :: (Eq a) => a -> [a] -> Bool
elem'  _   []     = False
elem'  y   [x]    = y == x
elem'  y   (x:xs) = if(y == x) then True else elem' y xs

(!!) :: [a] -> Int -> a
(!!) []     _   = error ""
(!!) (x:xs) 0   = x
(!!) (x:xs) y   = (!!) xs (y-1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter f (x:xs) = if f x then x : filter f xs else filter f xs

map :: (a -> a) -> [a] -> [a]
map _  []     = []
map f  (x:xs) = f x : map f xs

cycle :: [a] -> [a]
cycle []  = []
cycle xs  = xs ++ cycle xs

repeat :: a -> [a]
repeat a  = a : repeat a

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate y a = a : replicate (y-1) a

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf []     _      = True
isPrefixOf (x:xs) (y:ys) = if(x==y) then isPrefixOf xs ys else False

isInfixOf  :: (Eq a) => [a] -> [a] -> Bool
isInfixOf  []     _      = True
isInfixOf  xs     ys     = elem xs (combinations ys)

last :: [a] -> a
last []     = error ""
last [x]    = x
last (x:xs) = last xs

isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf []     _      = True
isSuffixOf (x:xs) (y:ys) = if(x'==y') then isSuffixOf xs ys else False
    where
        x' = last (x:xs)
        y' = last (y:ys)

zip :: [a] -> [b] -> [(a,b)]
zip _      []     = []
zip []     _      = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


zipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith _      []    _       = []
zipWith _      _     []      = []
zipWith f      (x:xs) (y:ys) = (f x y) : zipWith f xs ys

interpense :: a -> [a] -> [a]
interpense _ []     = []
interpense x [y]    = [y]
interpense x (y:ys) = y : (x: (interpense x ys))

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (interpense xs xss)

nub :: (Eq a) => [a] -> [a]
nub []        = []
-- nub (x:xs)    = if(elem x ) then nub xs else x:nub xs

splitAt :: Int -> [a] -> ([a],[a])
splitAt _ [] = ([],[])
splitAt y xs = (take y xs, drop y xs)

break :: (a -> Bool) -> [a] -> ([a],[a])
break _ []     = ([],[])
break p (x:xs) = if p x then (x:xs',xs'') else ([x],xs)
    where (xs',xs'') = break p xs

lines :: String -> [String]
lines ""       = []
-- lines (x:y:xs) = map  

-- words

unlines :: [String] -> String
unlines []     = []
unlines [x]    = x
unlines (x:xs) = x ++ "\n" ++ unlines xs

unwords :: [String] -> String
unwords []     = []
unwords [x]    = x
unwords (x:xs) = x ++ " " ++ unwords xs

transpose :: [[a]] -> [[a]]
transpose []     = []
-- transpose (x:xs) = 

palindrome :: String -> Bool
palindrome xs = xs == reverse xs 
