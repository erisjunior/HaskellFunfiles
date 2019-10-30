module Funktor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const

  -- ALGEBRAIC LAWS
  -- fmap id      = id
  -- fmap (f . g) = fmap f . fmap g
         

instance Funktor [] where
  fmap = map

instance Funktor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Funktor ((->) r) where
  fmap = (.)
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)

instance Funktor (Either a) where
  fmap _ (Left  x) = Left x
  fmap f (Right x) = Right (f x)

instance Funktor ((,) a) where
  fmap f (x,y) = (x, f y)

instance Funktor IO where
    fmap f ax  = 
        do  x <- ax
            return $ f x
