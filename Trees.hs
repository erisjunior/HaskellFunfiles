module Trees where

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
            deriving (Show , Eq)

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node x y z) = flatten y ++ [x] ++ flatten z

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf x)     = Leaf (f x)
tmap f (Node x y z) = Node (f x) (tmap f y) (tmap f z)

