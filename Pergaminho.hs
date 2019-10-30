module Pergaminho where

putTwo :: Char -> Char -> IO ()
putTwo x y =
    do putChar x 
       putChar y

getThreeFL :: IO (Char, Char)
getThreeFL =
    do x <- getChar
       getChar
       z <- getChar
       return (x, z) 
