module ExHyper where

import Prelude hiding ( exp )

-- Nat datatype --------------------------------

data Nat = Z | S Nat
     deriving (Eq, Show)

instance (Num Nat) where
    (+) = add
    (*) = mul
    abs = id
    fromInteger 0 = Z
    fromInteger n
      | n > 0     = S $ fromInteger (n-1)
      | otherwise = Z
    signum Z = Z
    signum n = S Z
    negate n = Z

instance (Ord Nat) where
    Z     <= m     = True
    (S n) <= Z     = False
    (S n) <= (S m) = n <= m

{- explicit definitions of add, mul, exp: 

add n Z     = n
add n (S m) = S (add m n)

mul n Z     = Z
mul n (S m) = add (mul n m) n

exp n Z     = S Z
exp n (S m) = mul (exp n m) n

-}

------------------------------------------------

-- substitute 'undefined' by the correct number
-- to define each of those functions:

add :: Nat -> Nat -> Nat
add = hyper 1

mul :: Nat -> Nat -> Nat
mul = hyper 2

exp :: Nat -> Nat -> Nat
exp = hyper 3

-- hyper n should return the n'th operation in the sequence:
-- (..?..), add, mul, exp, ...?

hyper :: Integral i => i -> (Nat -> Nat -> Nat)
hyper 1 n Z     = n
hyper 2 n Z     = Z
hyper 3 n Z     = S Z
hyper x n (S m) = case x of 1 -> S (hyper 1 m n)
                            2 -> hyper 1 (hyper 2 n m) n
                            3 -> hyper 2 (hyper 3 n m) n
