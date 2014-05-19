module Cours3 where

import Data.Maybe
import Control.Applicative
import Control.Monad hiding ( guard )

--Monad
--    bind
--    return
--    fail

guard :: Monad m => Bool -> m ()
guard test = if test then return () else fail "bad guard"

greaterThan30 x y = do
    a <- x
    b <- y
    guard $ a * b > 30
    return (a, b)
-- do is equivalent as
--x >>= (\a ->
--        y >>= (\b ->
--                guard (a * b > 30) >>= (\ ->
--                                        return (a, b))))

--data (Monoid w) => Write w a = {runWriter :: (a, w)}

-- e for environnment

data Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader r) where
--    fmap (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader r) = Reader $ \e -> f (r e)

instance Monad (Reader e) where
    return = Reader . const -- const ignores the second argument.
    -- return x = Reader $ const x
    (Reader r) >>= f = Reader $ \e -> runReader (f $ r e) e

ask :: Reader e e
ask = Reader id

asks :: Eq x => x -> Reader [(x, v)] (Maybe v)
asks k = do
    env <- ask
    return $ lookup k env

--hello :: Reader [(String, String)] String
--hello = do
--    env <- ask
--    let firstName = fromJust $ (lookup "first" env)
--    return ("Hello " ++ firstName)
--
--politeHello :: Reader [(String, String)] String
--politeHello = do
--    env <- ask
--    let firstName = fromJust $ (lookup "last" env)
--    return ("Hello " ++ firstName)
--
--hello2 = do
--    greetings <- hello
--    return greetings
--
--ask >>= \env ->
--    let firstName = fromJust $ (lookup "first" env)
--    return ("Hello") ++ firstName
--        >>= (\greetings -> return greetings)
hello :: Reader [(String, String)] String
hello = do
    firstName <- asks "first"
    return ("Hello " ++ fromJust firstName)


data State s a = State { runState :: s -> (a, s) }
instance Functor (State s) where
    fmap f (State rs) = State $ \s -> let (a, s') = rs s
                                      in (f a, s')

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State rs) >>= f = State $ \s ->
                                let (a, s') = rs s
                                in let (State rs') = f a
                                    in rs' s'
    -- rs:    s ------> (a, s')
    --     f: a ->   (t -> (b, t'))
    --         u ->  (b, (s->s')->(t->t'))

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put x = State $ \s -> ((), x)

change :: (s -> s) -> State s
change f = do
    current <- get
    put $ f current

instance Applicative (State s) where
    pure = return
    (<*>) = ap


fact :: Int -> State Int Int
fact 0 = do
    put 0
    return 1
fact n = do
    t <- fact(n-1)
    change (+1)
    return $ n * t

dummy :: State Int ()
dummy = do
    put 1
    put 2

-- RPN calculator with State
plus :: State [Int] ()
plus = do
    (b : a : xs) <- get
    put $ (a+b) xs

push :: Int -> State [Int] ()
push x = change (x:)

main = do
    push 3
    push 4
    push 5
    plus
    plus
