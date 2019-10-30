module IOwarmup where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    )

-- what is the type?  (don't cheat)
whatIsMyType :: IO [Char]
whatIsMyType =
    do v1 <- getChar
       v2 <- getChar
       putStrLn $ "\nI have all I need:" ++ [v1,v2]
       return [v1,v2]

putStr :: String -> IO ()
putStr []     = return ()
putStr (x:xs) = do
    putChar x
    putStr xs

lnize :: (a -> IO b) -> a -> IO b
lnize f x = do
    s <- f x
    putChar '\n'
    return s

putStrLn :: String -> IO ()
putStrLn = lnize putStr

getLine :: IO String
getLine = do
    c <- getChar
    if c == '\n' 
        then return ""
        else do l <- getLine
                return (c:l)

putNtimes :: Integral i => i -> Char -> IO ()
putNTimes 0 _ = return ()
putNTimes n x = do
    putChar x
    putNTimes (n-1) x

doNtimes :: Integral i => i -> IO a -> IO [a]
doNtimes 0 _  = return []
doNTimes n fx = do
    fx
    do doNTimes (n-1) fx

doForever :: IO a -> IO ()
doForever fx = do
    fx
    do doForever fx

when :: Bool -> IO () -> IO ()
when False _  = return ()
when _     fx = fx

-- consult read.txt to learn about read
getInteger :: IO Integer
getInteger = do
    s <- getLine
    return $ read s

