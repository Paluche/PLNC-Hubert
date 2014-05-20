-- Notes Courses
-- module Cours3 where
--
-- import Data.Maybe
-- import Control.Applicative
-- import Control.Monad hiding ( guard )
--
-- --Monad
-- --    bind
-- --    return
-- --    fail
--
-- guard :: Monad m => Bool -> m ()
-- guard test = if test then return () else fail "bad guard"
--
-- greaterThan30 x y = do
--     a <- x
--     b <- y
--     guard $ a * b > 30
--     return (a, b)
-- -- do is equivalent as
-- --x >>= (\a ->
-- --        y >>= (\b ->
-- --                guard (a * b > 30) >>= (\ ->
-- --                                        return (a, b))))
--
-- --data (Monoid w) => Write w a = {runWriter :: (a, w)}
--
-- -- e for environnment
--
-- data Reader e a = Reader { runReader :: e -> a }
--
-- instance Functor (Reader r) where
-- --    fmap (a -> b) -> Reader r a -> Reader r b
--     fmap f (Reader r) = Reader $ \e -> f (r e)
--
-- instance Monad (Reader e) where
--     return = Reader . const -- const ignores the second argument.
--     -- return x = Reader $ const x
--     (Reader r) >>= f = Reader $ \e -> runReader (f $ r e) e
--
-- ask :: Reader e e
-- ask = Reader id
--
-- asks :: Eq x => x -> Reader [(x, v)] (Maybe v)
-- asks k = do
--     env <- ask
--     return $ lookup k env
--
-- --hello :: Reader [(String, String)] String
-- --hello = do
-- --    env <- ask
-- --    let firstName = fromJust $ (lookup "first" env)
-- --    return ("Hello " ++ firstName)
-- --
-- --politeHello :: Reader [(String, String)] String
-- --politeHello = do
-- --    env <- ask
-- --    let firstName = fromJust $ (lookup "last" env)
-- --    return ("Hello " ++ firstName)
-- --
-- --hello2 = do
-- --    greetings <- hello
-- --    return greetings
-- --
-- --ask >>= \env ->
-- --    let firstName = fromJust $ (lookup "first" env)
-- --    return ("Hello") ++ firstName
-- --        >>= (\greetings -> return greetings)
-- hello :: Reader [(String, String)] String
-- hello = do
--     firstName <- asks "first"
--     return ("Hello " ++ fromJust firstName)
--
--
-- data State s a = State { runState :: s -> (a, s) }
-- instance Functor (State s) where
--     fmap f (State rs) = State $ \s -> let (a, s') = rs s
--                                       in (f a, s')
--
-- instance Monad (State s) where
--     return x = State $ \s -> (x, s)
--     (State rs) >>= f = State $ \s ->
--                                 let (a, s') = rs s
--                                 in let (State rs') = f a
--                                     in rs' s'
--     -- rs:    s ------> (a, s')
--     --     f: a ->   (t -> (b, t'))
--     --         u ->  (b, (s->s')->(t->t'))
--
-- get :: State s s
-- get = State $ \s -> (s, s)
--
-- put :: s -> State s ()
-- put x = State $ \s -> ((), x)
--
-- change :: (s -> s) -> State s
-- change f = do
--     current <- get
--     put $ f current
--
-- instance Applicative (State s) where
--     pure = return
--     (<*>) = ap
--
--
-- fact :: Int -> State Int Int
-- fact 0 = do
--     put 0
--     return 1
-- fact n = do
--     t <- fact(n-1)
--     change (+1)
--     return $ n * t
--
-- dummy :: State Int ()
-- dummy = do
--     put 1
--     put 2
--
-- -- RPN calculator with State
-- plus :: State [Int] ()
-- plus = do
--     (b : a : xs) <- get
--     put $ (a+b) xs
--
-- push :: Int -> State [Int] ()
-- push x = change (x:)
--
-- main = do
--     push 3
--     push 4
--     push 5
--     plus
--     plus
--     pNothing

module Cours3 where

import Control.Applicative
import Control.Monad hiding (guard)
import Data.Maybe

-- With guard, you can interrupt a computation if it does
-- not fulfill a condition. Note that the standard Haskell
-- library already has a (more restricted) guard implementation.

guard :: Monad m => Bool -> m ()
guard test = if test then return () else fail "bad guard"

-- guard lets us stop the computation below if the product of
-- the content of x and y is not greater than 30. You can test
-- it with: greaterThan30 [1..5] [1..10]
greaterThan30 :: (Monad m, Ord a, Num a) => m a -> m a -> m (a, a)
greaterThan30 x y = do
    a <- x
    b <- y
    guard $ a * b > 30
return (a, b)

-- Nothing magical here: this gets expanded like this
-- x >>= (\a ->
          y >>= (\b ->
                 --         --                 guard (a * b > 30) >>= (\ ->
                                                                       --         --                                          return (a, b))))

-- The Reader monad lets you retrieve an environment from within
-- the computation.
data Reader e a = Reader { runReader :: e -> a }

-- A reader is a functor, which ignores the environment
instance Functor (Reader e) where
fmap f (Reader r) = Reader $ \ e -> f (r e)

-- A reader is a monad, which propagates the environment untouched
-- along the computation chain.
instance Monad (Reader e) where
  return = Reader . const
    (Reader r) >>= f = Reader $ \e -> runReader (f $ r e) e

-- ask lets you retrieve the environment.
ask :: Reader e e
ask = Reader id

-- asks lets you retrieve a particular entry if the environment is
-- a list of (key, value) pairs.
asks :: Eq x => x -> Reader [(x, v)] (Maybe v)
asks k = do
env <- ask
return $ lookup k env

-- Here is a (particularly non-interesting) example of an environment
-- retrieval.
hello :: Reader [(String, String)] String
hello = do
firstName <- asks "first"
return ("Hello " ++ fromJust firstName)

-- The following code is useless (hello2 = hello would do), but
-- illustrates the fact that we do not need to pass the environment
-- around. The computation is done "within" the reader monad, thanks
-- to bind which takes care of propagating the environment around all
-- function calls.
hello2 :: Reader [(String, String)] String
hello2 = do
greetings <- hello
return greetings

-- Classical Monad => Applicative transformation.
instance Applicative (Reader r) where
    pure = return
    (<*>) = ap

-- The state monad combines the reader and the writer: an initial
-- state is passed around, but can be read and modified at will.
data State s a = State { runState :: s -> (a, s)  }

-- The state monad is a functor, which lets the state transformation
-- functionality untouched.
instance Functor (State s) where
  fmap f (State rs) = State $ \s -> let (a, s') = rs s in (f a, s')

-- The state monad is a monad (hence the name). return lets the
-- state untouched, while bind combines the two state transformers
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State rs) >>= f = State $ \s ->
                        let (a, s') = rs s
                        in let (State rs') = f a
                        in rs' s'

-- Classical Monad => Applicative transformation.
instance Applicative (State s) where
    pure = return
    (<*>) = ap

-- get retrieves the state.
get :: State s s
get = State $ \s -> (s, s)

-- put sets the state.
put :: s -> State s ()
put x = State $ \s -> ((), x)

-- change modifies the state (read/modify/write).
change :: (s -> s) -> State s ()
change f = do
    current <- get
    put $ f current

-- A factorial function which stores the number of
-- recursive operations. Try it with:
runState (fact 5) 0  => (120, 5)

fact :: Int -> State Int Int
fact 0 = do
    put 0
    return 1
    fact n = do
    t <- fact (n-1)
    change (+1)
    return $ n * t

-- A Fibonacci function which stores the number of
-- recursive operations. Note that we do not reset
-- the state here as was done in the factorial function,
-- so this gets added to the initial state.
fibo :: Int -> State Int Int
fibo 0 = return 1
fibo 1 = return 1
fibo n = do
    x <- fibo (n-1)
    y <- fibo (n-2)
    change (+2)
    return $ x + y

-- It is often more elegant not to use do (which smells of
-- imperative style) and use ">>=" or ">>" to combine
-- operations. ">>" as signature "Monad m => m a -> m b -> m b"
-- and can be defined as:
-- m >> f = m >>= (\_ -> f)
dummy :: State Int ()
dummy = put 1 >> put 2
-- How would we use the state monad to implement a stack-based
-- language? Let's add the stack top two values.
plus :: State [Int] ()
plus = change $ \ (b : a : xs) -> (a+b) : xs

-- Push a value onto the stack. Now, we can run:
--   runState (push 3 >> plus) [10, 20, 30]   =>  ((), [13, 20, 30])
-- Note that we ignore completely the value here, we are only
-- interested in the state itself.
push :: Int -> State [Int] ()
push x = change (x:)

-- This can be written more elegantly as:
--    push 3 >> push 4 >> push 5 >> plus >> plus
plusplus = do
    push 3
    push 4
    push 5
    plus
    plus
