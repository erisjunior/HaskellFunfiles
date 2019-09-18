module ExNat where

import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

data Nat = Zero | Succ Nat

instance Show Nat where
    show Zero     = "O"
    show (Succ x) = 'S' : show x

instance Eq Nat where

    (==) Zero     Zero     = True
    (==) Zero     _        = False
    (==) _        Zero     = False
    (==) (Succ m) (Succ n) = (==) m n

--
instance Ord Nat where

    (<=) Zero     Zero     = True
    (<=) Zero     _        = True
    (<=) _        Zero     = False
    (<=) (Succ m) (Succ n) = (<=) m n

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

  --  min = undefined

  --  max = undefined

instance (Enum Nat) where

    succ = Succ

    pred Zero     = Zero
    pred (Succ x) = x 

    toEnum = toNat
    fromEnum = fromNat

    enumFrom x = x : enumFrom (succ x)
    enumFromTo x y = 
        if x == y
        then [x] 
        else x : enumFromTo (succ x) y

    enumFromThen x y = x : enumFromThen y (x <+> y)

    enumFromThenTo x t y = 
        if y <= x
        then [x]
        else x: enumFromThenTo (x <+> t) t y
 

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

--
even :: Nat -> Bool
even = undefined
-- even Zero  = True
-- even m     = if ((<%> m (Succ $ Succ Zero) == Zero) then True else False)

--
odd :: Nat -> Bool
odd = undefined
-- odd  m     = if ((<%> m (Succ $ Succ Zero) == (Succ Zero)) then True else False)

(<+>) :: Nat -> Nat -> Nat
(<+>) n Zero     = n
(<+>) Zero n     = n
(<+>) (Succ n) m = Succ ((<+>) n m)

(<->) :: Nat -> Nat -> Nat
(<->) n        Zero     = n
(<->) Zero     _        = Zero
(<->) (Succ n) (Succ m) = (<->) n m

(<*>) :: Nat -> Nat -> Nat
(<*>) Zero        _           = Zero
(<*>) _           Zero        = Zero
(<*>) (Succ Zero) x           = x
(<*>) x           (Succ Zero) = x
(<*>) n           (Succ m)    = (<+>) n ((<*>) n m)

(<^>) :: Nat -> Nat -> Nat
(<^>) Zero  _        = Zero
(<^>) _     Zero     = Succ Zero
(<^>) n     (Succ m) = (<*>) n ((<^>) n m)

--
(</>) :: Nat -> Nat -> Nat
(</>) = undefined

--
(<%>) :: Nat -> Nat -> Nat
(<%>) Zero _            = Zero
(<%>) _    Zero         = error "Não se divide por zero"
--(<%>) (Succ n) (Succ m) = ()

--
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


absDiff :: Nat -> Nat -> Nat
absDiff Zero     m        = m
absDiff m        Zero     = m
absDiff (Succ m) (Succ n) = absDiff m n

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero     = Succ Zero
factorial (Succ m) = (<*>) (Succ m) (factorial m) 

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined

toNat :: Integral a => a -> Nat
toNat 0     = Zero
toNat x     = Succ (toNat (x-1))

fromNat :: Integral a => Nat -> a
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n

instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
    --
        | x < 0     = error ""
        | x == 0    = Zero
        | otherwise = Succ (fromInteger (x-1))

