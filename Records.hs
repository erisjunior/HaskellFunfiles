module Records where

type Name = String
type Age  = Int
data Sex  = Male | Female
    deriving ( Show, Eq )

data Person = Person { name :: Name
                     , sex  :: Sex
                     , age  :: Age
                     }
     deriving ( Show )



--data Person = Person Name Sex Age
--    deriving ( Show )

-- getters

--name :: Person -> Name
--name (Person n _ _) = n

--sex  :: Person -> Sex
--sex  (Person _ s _) = s

--age  :: Person -> Age
--age  (Person _ _ a) = a

--mkPerson :: Name -> Sex -> Age -> Person
--mkPerson n s a
--    | a > 100   = error "Muito velho"
--    | otherwise = Person n s a

--updateName :: Person -> Name -> Person
--updateName (Person _ s a) n = Person n s a
