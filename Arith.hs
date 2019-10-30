module Arith where

import Prelude hiding (iterate)

data Arith = Plus Arith Arith
           | Times Arith Arith
           | Atom Integer
           deriving ( Eq )

instance (Show Arith) where
    show (Atom  n)    = show n
    show (Times n m)  = showOp " * " n m
    show (Plus  n m)  = showOp " + " n m

showOp :: String -> Arith -> Arith -> String
showOp s n m = parenthesize $ show n ++ s ++ show m

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

val :: Arith -> Arith 
val = last . tillRep . iterate step
{-
val (Atom  n)    = n
val (Times n m)  = val n * val m 
val (Plus  n m)  = val n + val m
-}

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

tillRep :: Eq a => [a] -> [a]
tillRep []        = []
tillRep [x]       = [x]
tillRep (x:x':xs) = if x == x' then [x] else x : tillRep (x':xs)

step :: Arith -> Arith
step (Atom  n)                 = Atom   n
step (Times (Atom n) (Atom m)) = Atom  (n * m)
step (Times (Atom n) m)        = Times (Atom n) (step m)
step (Times n        m)        = Times (step n) m
step (Plus  (Atom n) (Atom m)) = Atom  (n + m)
step (Plus  (Atom n) m)        = Plus  (Atom n) (step m)
step (Plus  n        m)        = Plus  (step n) m
