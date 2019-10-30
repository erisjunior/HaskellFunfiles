module ExDrunk
    ( atIndices
    , everyOther
    , disjoint
    , stretch
    , drunk
    ) where



atIndices :: Integral i => [i] -> [a] -> [a]
atIndices []     _  = []
atIndices _      [] = []
atIndices (n:ns) xs = takeOne n xs ++ atIndices ns xs

takeOne :: Integral i => i -> [a] -> [a]
takeOne _ []     = []
takeOne 0 (x:xs) = [x]
takeOne n (x:xs) = takeOne (n-1) xs

everyOther :: Int -> [a] -> [a]
everyOther _ []     = []
everyOther n (x:xs) = x : everyOther n (drop (n-1) xs)

-- examples:
-- disjoint [1,5,9] [2 .. 6]
-- = False
-- disjoint [1,5,9] [2,4 ..]
-- = True
-- ASSUMPTIONS FOR disjoint xs ys:
--   xs and ys are sorted
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint []     _  = True
disjoint _      [] = True
disjoint (x:xs) ys = if has x ys then False else disjoint xs ys

has :: Ord a => a -> [a] -> Bool
has _ []     = False
has x (y:ys) = if x == y then True else has x ys

stretch :: Int -> [a] -> [a]
stretch _ []     = []
stretch n (x:xs) = replicate n x ++ stretch n xs

-- example:
-- drunk 3 "Gustavo"
-- = "GusGtuasvtoavo"
-- drunk 5 "Gustavo"
-- = "GustaGvuostavo"
-- To understand these string, either get drunk or look at the markings:
--       , , , , ,,,
--   "GusGtuasvtoavo"
--    ''' ' ' ' '
--         , , ,,,,,
--   "GustaGvuostavo"
--    ''''' ' '
drunk :: Int -> [a] -> [a]
drunk n xs = take n xs ++ drunkTake n xs ++ drop ((length xs) - n) xs

drunkTake :: Int -> [a] -> [a]
drunkTake n (x:xs)
    | length (x:xs) < n+1 = []
    | otherwise           = x : xs !! (n-1): drunkTake n xs

