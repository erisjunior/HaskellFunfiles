module ExMaybe where

import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)
--firstThat :: (a -> Bool) -> [a] -> Maybe a
--firstThat f []     = Nothing
--firstThat f (x:xs) = if f x then Just x else firstThat f xs

--
--isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
--isGoodFirstThat _ _ []      = Nothing
--isGoodFirstThat f g (x:xs)  =
--    if g x
--    then Just $ f x
--    else isGoodFirstThat f g xs

firstThat' :: (a -> Bool) -> [a] -> Maybe a
firstThat' p = listToMaybe . filter p

maybeize :: (a -> b) -> Maybe a -> Maybe b
maybeize f Nothing  = Nothing
maybeize f (Just x) = Just $ f x

isGoodFirstThat' :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat' f g = maybeize f . firstThat' g

catMaybes :: [Maybe a] -> [a]
catMaybes []              = []
catMaybes (Nothing  : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

fromJust :: Maybe a -> a
fromJust (Just m) = m
fromJust Nothing  = error "Nothing"

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust _        = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing  = x
maybe _ f (Just m) = f m

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = [ ]
maybeToList (Just m) = [m]

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith []           xs     = xs
tryToModifyWith (Nothing:ms) (x:xs) = x   : tryToModifyWith ms xs
tryToModifyWith (Just f:ms)  (x:xs) = f x : tryToModifyWith ms xs
