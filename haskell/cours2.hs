-- Notes
--module Cours2 where
--
--import Control.Monad
--import Data.Monoid
--import Prelude
--
---- Factoriel
---- ZipWith
---- take: return ONly the number specified of  the result list
---- zipWith applyes
---- take 10 $ zipWith (*) [1..] [1..]
--
----facts :: [Integer]
----facts = 1 : zipWith (*) facts [1..]
----
----
------ [] !! n prends l'élément n de la liste
------
----fact = (facts !!)
----
------ Fibonacci
------ Brut force
----fibo 0 = 1
----fibo 1 = 1
----fibo n = fibo (n - 1) + fibo ( n -2)
----
----fibos = 1 : 1 : zipWith (+) fibos (tail fibos)
----
----fibo' = (fibos !!)
----
----data Maybe a = Nothing | Just a
----
----instance Functor Maybe where
----    fmap _ Nothing = Nothing
----    fmap f (Just x) = Just (f x)
----
----
----instance Applicative Maybe where
----    pure = Just
----    Nothing <*> _ = Nothing
----    _ <*> Nothing = Nothing
----    (Just f) <*> (Just x) = Just (f x)
----
------ Bind
----(>>=) :: Monad m => m a -> (a -> m b) -> m b
--
--fathers = [("Daniel", "Jacques"), ("Jacques", "Toto"), ("Toto", "Oldelaf")]
--
--
--father :: String -> Maybe String
----father x = lookup x fathers
--father = flip lookup fathers
--
----grandfather x = fmap father (father x)
---- Will return Just Just <the father>
---- But bind will return Just < The father>
--
--grandfather x = father x >>= father
--grandgrandfathrer x = father x >>= father >>= father
--
--
----advlookup :: (MonadPlus m, Eq a) => a [(a, b)] -> m b
----advlookup _ [] = mzero
----advlookup a ((k, v) : xs) | a == k    = (return v) `mplus` advlookup a xs
----                          | otherwise = advlookup a xs
--
----class Monad m where
----    return :: a -> m a
----    fail   :: String -> m a
----    (>>=) m a -> (a -> m b) -> m b
----
----    fail = error
----
----instance Monad Maybe where
----    return = Just
----    fail _ = Nothing
----    Nothing >>= _ = Nothing
--
---- Map / zipWith / fmap
----
--
---- Need a backup `blabla`
----
---- fromJust Data.Maybe
--
--debug :: String -> ()
----debug s = (\_ -> ()) $ putStrLn s --Problem here putStrLn not evaluated
--debug s = seq (putStrLn s) ()
--
--echo = getLine >>= (\s -> putStrLn (s ++ " " ++ s))
--
--echo' = do
--    s <- getLine
--    putStrLn s
--    putStrLn s
--
---- do
----   xxx
----   yyy
----
----x >>=
--
--loop = do
--    s <- getLine
--    putStrLn (reverse s)
--    loop
--
--
--data Writer w a = Writer { runWriter :: (a, w) } deriving (Show)
--
--instance (Monoid w) => Monad (Writer w) where
--    return x = Writer $ (x, mempty)
--    Writer (a, w) >>= f = let Writer (a', w') = f a
--                          in Writer (a', w `mappend` w')
--
--tell :: (Monoid w) => w -> Writer w ()
--tell x = Writer ((), x)
--
--
--fact :: Integer -> Writer [String] Integer
--fact 0 = do
--    tell ["Computing fact 0"]
--    return 1
--
--fact n = do
--    t <- fact (n-1)
--    tell $ ["Recursed from n == " ++ show n]
--    return $ n * t
----fact (n - 1) >>= (\t -> tell $ ["..."] >>= (\_ -> return $ n * t))
--
--compute :: Writer w a -> a
--compute = fst.runWriter
--
--logs :: Writer w a -> w
--logs = snd.runWriter
--
--displayLogs :: Writer [String] a -> IO ()
--displayLogs m = sequence_ $ map putStrLn logs m


-- Complete By Samuel Tardieu
module Cours2 where

import Control.Applicative
import Control.Monad
import Data.Monoid

-- Here, we define facts as an infinite list of factorials. Like a recursion,
-- you only have to give the initial value and explain how to go on from there.
facts :: [Integer]
facts = 1 : zipWith (*) facts [1..]

-- Implementing a factorial function becomes trivial.
fact = (facts !!)

-- Let's try this (slooow) recursive version of the Fibonacci sequence.
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- And let's do it with an elegant and much faster definition.
fibos = 1 : 1 : zipWith (+) fibos (tail fibos)

-- Here we are.
fibo' = (fibos !!)

-- We did the test with fathers, but I prefer the term parent.
parents = [("Daniel", "Jacques"), ("Jacques", "Toto"),
           ("Toto", "Oldelaf"), ("Oldelaf", "Bison fute"),
           ("Marcel", "Giedre"), ("Marcel", "Daniel"),
           ("Giedre", "Jean"), ("Giedre", error "Giedre has no second parent")]

-- The standard lookup is too narrow: Eq a => a -> [(a, b)] -> Maybe b
-- It forces you to get one answer at most. Let's redefine another lookup
-- which lets you choose the kind of MonadPlus you want to store your result
-- into.
advlookup :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
advlookup _ [] = mzero
advlookup a ((k, v) : xs) | a == k    = (return v) `mplus` advlookup a xs
                          | otherwise = advlookup a xs

-- We can check that we can get back the limited version by forcing the
-- types to be restricted to the same ones as lookup.
lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' = advlookup

-- Lookup one or several parents. You can coerce the result into a single result
-- by using (parent x :: Maybe String), or get all the results with (parent x :: [String]).
parent :: MonadPlus m => String -> m String
parent = flip advlookup parents

-- You can query for the grandparents the same way, by using >>= (bind) on
-- the parent in order to chain the calls.
grandparent x = parent x >>= parent


-- You can go on forever!
grandgrandparent x = parent x >>= parent >>= parent

-- The Monad typeclass looks like that (+ the >> operator which was not seen in class)
-- with instances for Maybe and lists.
--
-- class Monad m where
--   return :: a -> m a
--   fail :: String -> m a
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--
--   fail = error
--   a >> b = a >>= (\_ -> b)
--
-- instance Monad Maybe where
--   return = Just
--   fail _ = Nothing
--   Nothing  >>= _ = Nothing
--   (Just x) >>= f = f x
--
-- instance Monad [] where
--   return x = [x]
--   fail _   = []
--   l >>= f  = concat [f(x) | x <- l]

-- IO operations all work in the IO monad
debug :: String -> IO ()
debug = putStrLn

-- echo and echo' are equivalent…
echo = getLine >>= (\s -> putStrLn (s ++ s))

-- …thanks to the "do" notation
echo' = do
  s <- getLine
  putStrLn (s ++ s)

-- Let's print each line twice, …
echo'' = do
  s <- getLine
  putStrLn s
  putStrLn s

-- … which is equivalent to this code.
echo''' = getLine >>= (\s -> putStrLn s >>= (\_ -> putStrLn s))

-- Now, we print the reverse of strings entered by the user.
loop = do
  s <- getLine
  putStrLn (reverse s)
  loop

-- The Writer monad depends on two types. a is the type of the data
-- stored into the monad, and w is the type of the additional information
-- (decoration) which is attached to the monad.
--
-- Its main principle is that computations will append previously existing
-- data with data provided by the function called by >>=. This is the role
-- of >>= too take care of appending all decoration data.

-- We also show the way of naming fields, and obtaining an accessor to them.
data Writer w a = Writer { runWriter :: (a, w) } deriving (Show)

-- A writer is a functor: the decoration is untouched by function application.
instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

-- A writer is an applicative: newer versions of GHC require this.
instance (Monoid w) => Applicative (Writer w) where
  pure = return
  (<*>) = ap

-- If w is a monoid (which can then start empty and gets appended to), then
-- (Writer w) is a monad which contains a value of type a (remember, Writer
-- has two types arguments, as in Writer w a).
instance (Monoid w) => Monad (Writer w) where
  -- return creates a new monad, with no decoration, just the value given by
  -- the user.
  return x = Writer $ (x, mempty)
  -- >>= calls the user provided function, takes the decoration out of the
  -- result and prepends it with the preexisting one.
  Writer (a, w) >>= f = let Writer (a', w') = f a
                        in Writer (a', w `mappend` w')

-- tell is the way to add some decoration. Using tell in the context of a Monad
-- will create a new writer with a content of () (unit) and the given decoration.
-- It will then be considered during subsequent bind calls.
tell :: w -> Writer w ()
tell x = Writer ((), x)

-- Let's write a factorial function which takes adds some decoration to the
-- computation. Without the "tell", that would be written as:
--
-- fact 0 = return 1
-- fact n = (*n) <$> fact (n-1)
--
-- Or as:
--
-- fact 0 = return 1
-- fact n = do
--   t <- fact (n-1)
--   return $ n * t
--
-- Here, we add some "tell" to give information about what is happening.
fact' :: Int -> Writer [String] Int
fact' 0 = do
  tell ["Computing fact 0"]
  return 1
fact' n = do
  t <- fact' (n-1)
  tell $ ["I just recursed for n == " ++ show n]
  return $ n * t

-- Write only the result of the computation, forgetting all about the decoration.
-- Use it as in: compute fact' 5
compute :: Writer w a -> a
compute = fst.runWriter

-- Only give the logs: logs $ fact' 5
logs :: Writer w a -> w
logs = snd.runWriter

-- Display the logs on standard output, one per line: displayLogs $ fact' 5
displayLogs :: Writer [String] a -> IO ()
displayLogs m = sequence_ $ map putStrLn $ logs m

-- Note that displayLogs can also be written using mapM_:
-- displayLogs = mapM putStrLn . logs
