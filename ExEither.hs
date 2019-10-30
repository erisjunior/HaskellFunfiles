module ExEither where

-- Do not alter this import!
import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft

left :: Either a b -> a
left (Left x) = x 
left _        = error "Não é left"

right :: Either a b -> b
right (Right x) = x
right _         = error "Não é right"

lefts :: [Either a b] -> [a]
lefts []     = []
lefts (x:xs) = if isLeft x then left x : lefts xs else lefts xs

rights :: [Either a b] -> [b]
rights []     = [] 
rights (x:xs) = if isRight x then right x : rights xs else rights xs

fromLeft :: a -> Either a b -> a
fromLeft x y = if isLeft y then left y else x

fromRight :: b -> Either a b -> b
fromRight x y = if isRight y then right y else x

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers (x:xs) = if isLeft x then (left x:x', y') else (x', right x:y')
    where (x', y') = partitionEithers xs

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g x = if isLeft x then f (left x) else g (right x) 
