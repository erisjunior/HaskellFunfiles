module Funky where

data Besta a = Coisa
             | Coiso
             | Algo a
     deriving ( Show, Eq )

instance Functor Besta where
    fmap f (Algo x) = Algo $ f x
    fmap _ Coiso    = Coiso
    fmap _ Coisa    = Coisa

nested :: [Maybe String]
nested = [Just "oi", Nothing, Just "hello"]

trocape :: a -> Char
trocape = const 'p'
