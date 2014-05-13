module Cours1 where

-- Github: samueltardieu
-- Bitbucket: rfc1149
-- Mail: sam@rfc1149.net

-- Some base types: Int, Integer, Char, Bool, Fractional, Unit

-- Since we redefine some standard data types and operators,
-- we need to mask them by explicitly importing the Prelude.
-- Otherwise, the Prelude would be automatically imported with
-- all its content.

import Prelude hiding (length, const, flip, Maybe(..), fmap, Functor,
                       ($), Applicative(..), pure, (<*>), zipWith)

-- We define a new function which takes an Int and returns an Int.
-- We would not have to specify the type, but here we are willing
-- to restrict it.
increment :: Int -> Int
increment x = x + 1

-- Implicit type declaration.
increment' x = x + 1

-- Illustration of the if/then/else construct.
incrementifnon0 x = if x == 0
                    then x
                    else x + 1

-- Recursive implementation of length using if/then/else.
length :: [a] -> Int
length l = if null l
           then 0
           else 1 + length (tail l)

-- Recursive implementation of length using pattern matching.
-- Note the use of the "dontcare" (_) which means that we are
-- not interested in naming the value since we will not use it.
-- We also use the "cons" operator ":" which builds a list from
-- its head and its tail, and can also be used for pattern matching.
length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- Illustration of case/of which uses explicitly required pattern.
-- matching.
length'' :: [a] -> Int
length'' l = case l of
  []       -> 0
  (_ : xs) -> 1 + length'' xs

-- This function takes a string and *never* returns an int, since it
-- calls error.
myfunc :: String -> Int
myfunc = error

-- This function also takes a string and also never returns an int, since
-- it loops indefinitely.
myfunc' :: String -> Int
myfunc' s = myfunc' s

-- This function has the same signature as "id", but takes forever to return.
myfakeid :: a -> a
myfakeid x = myfakeid x

-- This function has the same signature as "id", but returns with an error.
myfakeid' :: a -> a
myfakeid' x = error "I don't want to"

-- The "const" builtin function discards its second argument.
const :: a -> b -> a
const x _ = x

-- "flip" is useful to reverse the order of application.
flip :: (a -> b -> c) -> b -> a -> c
flip f y x = f x y

-- "second" discards its first element.
second :: a -> b -> b
second = flip const

-- Alternate implementation of "second". It works because "second a b"
-- is then extended into "const id a b". "const id _" is "id", so
-- the expression is really "id b" which is "b".
second' :: a -> b -> b
second' = const id

-- (.) is the "dot" operator to combine functions.
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

-- We define an algebraic data type which can either represent a
-- valid value (Just x) or no value (Nothing). We request the
-- automatic derivation from the Show typeclass: if the a type
-- belongs to the Show typeclass itself, we will be a member of it
-- as well using a predefined printing scheme (print the name of
-- the constructor, then the value).
data Maybe a = Just a | Nothing
  deriving (Show)

-- "f" is a "Functor" if we can apply functions within "f a" objects
-- by the way of "fmap".
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Maybe is a functor, as we know how to apply the function if we have
-- a content (Just x), or it we have none (just do nothing in this case).
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

-- Lists are functors too, you just have to apply the function to every
-- element of the list, that is… map!
instance Functor [] where
  fmap = map

-- Shortcut for infix notation.
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap
infixl 0 <$>

-- Shortcut for separating lengthy lines with less parentheses.
-- Writing "f x $ g y $ h z" is similar to "f x (g y (h z))".
($) :: (a -> b) -> a -> b
f $ x = f x
infixr 0 $

-- Binary tree type
data Tree a = Empty | Branch (Tree a) a (Tree a)
   deriving (Show)

-- The tree is a functor, just apply the function to every value stored
-- in the tree.
instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Branch left x right) = Branch (f <$> left) (f x) (f <$> right)

-- Demo tree to avoid retyping it every time. Not every interesting though.
tree = Branch (Branch Empty 3 (Branch Empty 4 Empty)) 17 (Branch Empty 8 Empty)

-- An applicative lets you apply a wrapped function onto a wrapped value.
class Applicative a where
  pure :: x -> a x
  (<*>) :: a (x -> y) -> a x -> a y
  infixl 0 <*>

-- Maybe is an applicative: if there is no function, apply none and return
-- Nothing, ditto for the value. Otherwise, just do it.
instance Applicative Maybe where
  pure = Just
  _ <*> Nothing = Nothing
  Nothing <*> _ = Nothing
  (Just f) <*> (Just x) = Just $ f x

-- Lists are applicative. Here we have two alternatives: either apply every
-- function to every value (à-la cartesian product), or apply the first function
-- to the first value, the second function to the second value, and so on (and
-- stop as soon as one of both lists is empty).
instance Applicative [] where
  pure x = [ x ]
  -- lf <*> lx = [f(x) | f <- lf, x <- lx]
  lf <*> lx = zipWith ($) lf lx

-- Redefinition of (+) operator restricted to small integers.
plus :: Int -> Int -> Int
plus = (+)

-- Combine two lists using a combination function. The first element of
-- one list will be combined through the function with the first element
-- of the other, and so on. It stops when either list is empty.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (l : ls) (r : rs) = f l r : zipWith f ls rs

-- Intra-functions
f a b = let sum = a + b
        in sum * sum
-- Or
f a b = sum * sum
        where sum = a + b
