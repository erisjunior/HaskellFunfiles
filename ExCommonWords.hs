module ExCommonWords
    ( commonWords
    ) where

-- commonWords 4 "Define the function commonWords,
-- which should receive an Int n and a text and
-- return some string describing the n most common words
-- found in the input text."
--
-- should return something like:
--
-- "the: 3\nand: 2\nn: 2\ntext: 2\n"

-- Use everything you've learnt so far to make your
-- program elegant and nice!

-- Your imports should go here.
-- Might want to make them qualified;
-- import only what you need from each module!

import Prelude hiding (Word)
import Data.Char
-- Let's start with some type synonyms you might want to use:

type Word = String
type Text = String

-- On with the functions now:

commonWords
    :: Int     -- how many common words
    -> Text    -- the input text (book, poem, whatever)
    -> String  -- the output string with the results

-- Let's break the text into wods
-- Then Create pairs, with the text and the total of times it shows up
-- Sort the pairs by the amount of times it shows up
-- And finally, show the how much result we want
commonWords n = showResult . take n . sort . removeDuplicates . createPairs . words . map (toLower)

-- creating pairs step
createPairs :: [Word] -> [(Word, Int)]
createPairs [] = []
createPairs ws = map (createPair ws) ws

createPair :: [Word] -> Word -> (Word, Int)
createPair _  [] = ([], 0)
createPair ws w  = (w , count)
    where count = length $ filter (==w) ws

-- clean when the word repeat
removeDuplicates :: [(Word, Int)] -> [(Word, Int)]
removeDuplicates []         = []
removeDuplicates ((w,n):xs) = (w,n) : removeDuplicates (hasDuplicates w xs)

hasDuplicates :: Word -> [(Word, Int)] -> [(Word, Int)]
hasDuplicates [] _           = []
hasDuplicates _  []          = []
hasDuplicates w  ((w',n):xs) = if w' == w then hasDuplicates w xs else (w',n) : hasDuplicates w xs

-- sort step
sort :: [(Word, Int)] -> [(Word, Int)]
sort []     = []
sort ((w,n):xs) = sort large ++ [(w,n)] ++ sort small
    where
        small = [ (w',n') | (w',n') <- xs, n' <= n ]
        large = [ (w',n') | (w',n') <- xs, n' >  n ]

-- showing step
showResult :: [(Word, Int)] -> String
showResult []         = []
showResult ((w,m):ws) = w ++ ": " ++ show m ++"\n" ++ showResult ws
