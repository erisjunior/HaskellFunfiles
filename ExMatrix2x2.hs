module ExMatrix2x2
    ( matrix
    , zero
    , identity
    , rows
    , cols
    , getElem
    , transpose
    , det
    , isDiagonal
    , isTriangular
    , isLowerTriangular
    , isUpperTriangular
    , singular
    , invertible
    , inverse
    ) where

type Number = Double
type Row = [Number]
type Col = [Number]

data Matrix2x2 = RMatrix (Row, Row) 
               | CMatrix (Col, Col)

instance Show Matrix2x2 where
    show (RMatrix (x, y)) = show (x, y)
    show (CMatrix (x, y)) = show (x, y)

instance Eq Matrix2x2 where
    (==) (CMatrix([a,b],[c,d])) (CMatrix([w,x],[y,z])) = (a==w && b==x && c==y && d==z)
    (==) (RMatrix([a,b],[c,d])) (RMatrix([w,x],[y,z])) = (a==w && b==x && c==y && d==z)
    (==) (RMatrix([a,b],[c,d])) (CMatrix([w,x],[y,z])) = (a==w && b==y && c==x && d==z)

instance Num Matrix2x2 where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

-- matrix a b c d should create the matrix
-- ( a c )
-- ( b d )
matrix :: Number -> Number -> Number -> Number -> Matrix2x2
matrix a b c d = CMatrix ([a,b],[c,d])

matrix :: Number -> Number -> Number -> Number -> Matrix2x2
matrixR a b c d = CMatrix ([a,c],[b,d])

zero :: Matrix2x2
zero = matrix 0 0 0 0

identity :: Matrix2x2
identity = matrix 1 1 1 1

rows :: Matrix2x2 -> [Row]
rows (CMatrix([a,b],[c,d])) = [[a,c],[b,d]]
rows (RMatrix([a,b],[c,d])) = [[a,b],[c,d]]

cols :: Matrix2x2 -> [Col]
cols (CMatrix([a,b],[c,d])) = [[a,b],[c,d]]
cols (RMatrix([a,b],[c,d])) = [[a,c],[b,d]]

getElem :: (Int,Int) -> Matrix2x2 -> Number
getElem (x,y) (CMatrix([a,b],[c,d]))
    | x == 0 && y == 0 = a
    | x == 0 && y == 1 = b
    | x == 1 && y == 0 = c
    | x == 1 && y == 1 = d
    | otherwise        = error ""
getElem (x,y) (RMatrix([a,b],[c,d]))
    | x == 0 && y == 0 = a
    | x == 0 && y == 1 = c
    | x == 1 && y == 0 = d
    | x == 1 && y == 1 = d
    | otherwise        = error ""

transpose :: Matrix2x2 -> Matrix2x2
transpose (RMatrix([a,b],[c,d])) = matrixR a c b d
transpose (CMatrix([a,b],[c,d])) = matrix  a c b d

det :: Matrix2x2 -> Number
det = undefined

isDiagonal :: Matrix2x2 -> Bool
isDiagonal = undefined

isTriangular :: Matrix2x2 -> Bool
isTriangular = undefined

isLowerTriangular :: Matrix2x2 -> Bool
isLowerTriangular = undefined

isUpperTriangular :: Matrix2x2 -> Bool
isUpperTriangular = undefined

singular :: Matrix2x2 -> Bool
singular (CMatrix([a,b],[c,d])) = a == b && b == c && c == d
singular (RMatrix([a,b],[c,d])) = a == b && b == c && c == d

invertible :: Matrix2x2 -> Bool
invertible = not . singular

inverse :: Matrix2x2 -> Matrix2x2
inverse :: Matrix2x2 -> Matrix2x2
inverse (RMatrix([a,b],[c,d])) = matrix  a c b d
inverse (CMatrix([a,b],[c,d])) = matrixR a c b d


