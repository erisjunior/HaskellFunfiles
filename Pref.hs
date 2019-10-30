module Pref where

import Prelude hiding ( foldl, foldr )

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _  base []     = base
foldr op base (x:xs) = x `op` foldr op base xs

soma :: Num a => [a] -> a
soma = soma' 0
    where
        soma' v []     = v
        soma' v (x:xs) = soma' (v+x) xs

foldl :: (a -> a -> a) -> a -> [a] -> a
foldl _  base []       = base
foldl op base (x:xs)   = (base `op` x) `op` foldl op x xs
