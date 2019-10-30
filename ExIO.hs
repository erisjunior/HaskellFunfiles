module IOkit where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    , interact
    , (>>)
    , (>>=)
    )

import System.IO
    ( hSetEcho
    , hGetEcho
    , stdin
    , stdout
    )

getLine :: IO String
getLine = do
    c <- getChar
    if isEOL c
       then return ""
       else do l <- getLine
               return (c:l)

isEOL :: Char -> Bool
isEOL = (== '\n')

getInt :: IO Int
getInt =
    do s <- getLine
       return $ read s

getSafeInt :: IO (Maybe Int)
getSafeInt =
  do s <- getLine
     let parsed = reads s :: [(Int, String)]
     case parsed of
       [(n, "")] -> return $ Just n
       _         -> return Nothing

-- sequencing: first do f ignoring its result, then do g and keep its result
infixl 1 >>

(>>) :: IO a -> IO b -> IO b
(>>) ax ay = do ax
                ay
pause :: IO ()
pause = (void . echoless) getChar

skip :: IO ()
skip = do return ()

newline :: IO ()
newline = do
    putChar '\n'

-- exercise: define it as a foldr
putStr :: String -> IO ()
putStr []     = skip
putStr (x:xs) = putChar x >> putStr xs

-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize f x = do
    s <- f x
    newline
    return s

putStrLn :: String -> IO ()
putStrLn = lnize putStr

putCharLn :: Char -> IO ()
putCharLn = lnize putChar

interact :: (String -> String) -> IO ()
interact f = do 
    x <- getLine
    putStr ( f x )

perlineize :: (String -> String) -> (String -> String)
perlineize f = unlines . map f . lines

interactPerLine :: (String -> String) -> IO ()
interactPerLine = interact . perlineize

when :: Bool -> IO () -> IO ()
when b ax = do if b then ax else skip

unless :: Bool -> IO () -> IO ()
unless = when . not

guard :: Bool -> IO ()
guard = undefined

forever :: IO a -> IO b
forever ax = do ax
                forever ax

(<<) = flip (>>)

void :: IO a -> IO ()
void = (>> return ())

-- Kleisli compositions
infixr 1 >=>, <=<

-- diagrammatic order
(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
(f >=> g) x = do
    x' <- f x
    g x'

-- traditional order
-- comparison of types:
-- (.)   :: (b ->    c) -> (a ->    b) -> a ->    c
-- (<=<) :: (b -> IO c) -> (a -> IO b) -> a -> IO c
(<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) = flip (>=>)


-- Bind
infixl 1 >>=

(>>=) :: IO a -> (a -> IO b) -> IO b
ax >>= f = do 
    x <- ax
    f x


infixl 4 $>, <$

-- make an action that has the side effects of the action on the left
-- but with result the value on the right
($>) :: IO a -> b -> IO b
($>) ax y = ax >> return y 

-- vice-versa
(<$) :: a -> IO b -> IO a
(<$)= flip ($>)

ap :: IO (a -> b) -> IO a -> IO b
af `ap` ax = do
    f <- af
    x <- ax
    return (f x)

filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO _ []     = return []
filterIO f (x:xs) = do
    b <- f x
    xs' <- filterIO f xs
    let xs'' = if b then x : xs' else xs'
    return xs''

iomap :: (a -> b) -> IO a -> IO b
iomap f ax = do 
    x <- ax
    return (f x)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f = sequenceIO . map f

mapIO_ :: (a -> IO b) -> [a] -> IO ()
mapIO_ = undefined

zipWithIO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithIO = undefined

zipWithIO_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithIO_ = undefined

sequenceIO :: [IO a] -> IO [a]
sequenceIO []       = return []
sequenceIO (ax:axs) = do
    x <- ax
    xs <- sequenceIO axs
    return (x:xs)

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ = void . sequenceIO 

replicateIO :: Integral i => i -> IO a -> IO [a]
replicateIO = undefined

replicateIO_ :: Integral i => i -> IO a -> IO [a]
replicateIO_ = undefined

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO = undefined

forIO_ :: [a] -> (a -> IO b) -> IO ()
forIO_ = undefined

joinIO :: IO (IO a) -> IO a
joinIO = undefined

foldlIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlIO = undefined

foldlIO_ :: (b -> a -> IO b) -> b -> [a] -> IO ()
foldlIO_ = undefined

echoless :: IO a -> IO a
echoless ax =
    do oldEcho <- hGetEcho stdin
       hSetEcho stdin False
       x <- ax
       hSetEcho stdin oldEcho
       return x

hangman :: IO ()
hangman = do putStrLn "Choose word:"
             word <- echoless getLine
             putStrLn "Try to guess it:"
             play word

play :: String -> IO ()
play word = do 
             putStrLn "? "
             guess <- getLine
             if guess == word
                then putStrLn "You got it!"
                else do putStrLn "Nope..."
                        play word
