module ExCH where

-- Bottom is a (the?) type with no proper values
data Bottom

-- Unit is a (the?) type with a unique proper value
data Unit = Unit
instance (Show Unit) where
    show Unit = "Unit"

-- What the hell is boom?!
type Boom a = a -> Bottom

-- Can you define proper inhabitants for the following types?

bb :: Bottom -> Bottom
bb x = x 

bu :: Bottom -> Unit
bu x = Unit

ub :: Unit -> Bottom
ub = undefined

uu :: Unit -> Unit
uu Unit = Unit 

nni :: a -> Boom (Boom a)
nni = undefined 

nne :: Boom (Boom a) -> a
nne = undefined

nnn :: Boom (Boom (Boom a)) -> Boom a
nnn = undefined

lem :: Either a (Boom a)
lem = undefined

lemnne :: Either a (Boom a) -> (Boom (Boom a)) -> a
lemnne = undefined

wlem :: Boom (Boom (Either a (Boom a)))
wlem = undefined

acnaib :: (a, Boom a) -> Bottom
acnaib = undefined

invimp :: (a -> b) -> (b -> a)
invimp = undefined

conpos :: (a -> b) -> (Boom b -> Boom a)
conpos = undefined

cpcp :: (Boom b -> Boom a) -> (Boom (Boom a) -> Boom (Boom b))
cpcp = undefined

climp :: (a -> b) -> Either (Boom a) b
climp = undefined

climp' :: Either (Boom a) b -> (a -> b)
climp' = undefined

nb :: (a, Boom a) -> Bottom
nb = undefined

exfq :: Bottom -> b
exfq = undefined

dem1 :: Boom (a,b) -> Either (Boom a) (Boom b)
dem1 = undefined

dem1' :: Either (Boom a) (Boom b) -> Boom (a,b)
dem1' = undefined

dem2 :: (Boom a, Boom b) -> Boom (Either a b)
dem2 = undefined

dem2' :: Boom (Either a b) -> (Boom a, Boom b)
dem2' = undefined


