module TD2 where

import Prelude
import Control.Arrow
import Control.Applicative
import Data.Ratio
import Data.Maybe
import Data.List

data Prob a = Prob [(a, Ratio Int)] deriving (Show)

instance Functor Prob where
    -- The function applies to the value and not the probability.
    fmap f (Prob x) = Prob $ map (first f) x

-- Return creates a une unique probabily and the value is so 1,
-- (>>=) applyes a function a the probabilities and combine the initials and
-- finals probabilities.
-- The result won't be canonized.
-- So for every couple (value, probability) we apply f to the value which
-- returns a (Prob a) then we must multiply all the probailities of the
-- resultant (Prob a) by the probability associated to the value.
--
-- Using the function map we will have list of (Prob a), we must concatenates
-- them into one list wich will be the result.

-- Concatenate a list of (Prob a) into a (Prob a)
concatProb :: [Prob a] -> Prob a
concatProb ((Prob x) : (Prob y) : []) = Prob (x ++ y)
concatProb ((Prob x) : (Prob y) : xs) = concatProb ((Prob (x ++ y)) : xs)

-- Multiply all the probabilities of a Prob by a Ratio Int
mulProb :: Ratio Int -> Prob a -> Prob a
mulProb a (Prob x) = Prob $ map (second (a*)) x

-- Realise the operation of applying to a couple (value, probability), the
-- function to the value, and multiply all the probabilties of the result by
lBindProb :: (a -> Prob b) -> (a, Ratio Int) -> Prob b
lBindProb f (x, p) = mulProb p $ f x

instance Monad Prob where
    return x = Prob [(x, 1)]
    (Prob x) >>= f = let v = map (lBindProb f) x
                     in concatProb v
    fail _   = Prob []

sameProbability :: [a] -> Prob a
sameProbability l = Prob $ map (\x -> (x, 1  % (length l))) l

-- Retrieve the probability of a value from a Prob a
probability :: Eq a => a -> Prob a -> Ratio Int
-- probOf v (Prob l) = lookup v l
--   Here lookup return a Maybe (Ratio Int) instead of Ratio Int wanted.
--   We need to get ride of the Maybe...
-- probOf v (Prob l) = fromJust $ lookup v l
--   Here we have no more Maybe, but an error if lookup return Nothing. But
--   the probability of something not in the list is 0.
probability v (Prob l) = fromMaybe 0 (lookup v l)

-- In case there is a double definition of the same value in the Prob a, we
-- want to gather the values.
-- Example: [(a, p), (b, p'), (a, p")] -> [(a, p+p"), (b, p')]
groupProb :: Ord a => [(a, Ratio Int)] -> [(a, Ratio Int)]
groupProb (x : [])     = [x]
groupProb (x : y : xs) =  if (fst x) == (fst y)
                          then groupProb ((fst x, (snd x) + (snd y)) : xs)
                          else x : groupProb (y : xs)

canonize :: Ord a => Prob a -> Prob a
canonize (Prob l) = Prob $ groupProb $ sort l

-- Test list for canonize.
testList = Prob [(1, 1%4), (1, 1%4), (1, 1%4), (2, 1%4)]
dice = sameProbability [1..6]

-- In order to check the probability to obtain twice the same number on two
-- dices.
double :: Prob Bool
double = do
    x <- dice
    y <- dice
    return $ x == y

-- In order to check the probability of having a number with the sum of two
-- dices.
pair :: Prob Int
pair = do
  x <- dice
  y <- dice
  return $ x + y

-- Let's play: cure or kill people with statistics.
-- Probability to be sick of the rare disease.
sick :: Prob Bool
sick = Prob [(True, 1%100000), (False, 99999%100000)]

-- Probability distribution of corresponding to the result of the test that
-- detect the disease. The test is reliable at 99.99% (999%1000)

positive :: Bool -> Prob Bool
positive True  = Prob [(True, 999%1000), (False, 1%1000)]
positive False = Prob [(True, 1%1000), (False, 999%1000)]

renormalize :: Prob a -> Prob a
renormalize = undefined

results :: Prob Bool
results = do
    x <- sick
    y <- positive x
    return $ y /= x
