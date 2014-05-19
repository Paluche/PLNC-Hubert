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
    fmap f (Prob l) = Prob $ map (first f) l

-- Retrieve the probability of a value from a Prob a
probOf :: Eq a => a -> Prob a -> Ratio Int
-- probOf v (Prob l) = lookup v l
--   Here lookup return a Maybe (Ratio Int) instead of Ratio Int wanted.
--   We need to get ride of the Maybe...
-- probOf v (Prob l) = fromJust $ lookup v l
--   Here we have no more Maybe, but an error if lookup return Nothing. But
--   the probability of something not in the list is 0.
probOf v (Prob l) = fromMaybe 0 (lookup v l)

-- In case there is a double definition of the same value in the Prob a, we
-- want to gather the values.
-- Example: [(a, p), (b, p'), (a, p")] -> [(a, p.p"), (b, p')]
-- The next function should be merge in the next one. It exists for the
-- function canonize.
groupProb :: Ord a => [(a, Ratio Int)] -> [(a, Ratio Int)]
groupProb (x : [])     = [x]
groupProb (x : y : xs) =  if (fst x) == (fst y)
                          then (fst x, (snd x) + (snd y)) : groupProb xs
                          else x : groupProb (y : xs)

canonize :: Ord a => Prob a -> Prob a
canonize (Prob l) = Prob $ groupProb $ sort l

--Prob $ let g = groupBy (\(x, _)  (y, _) -> x == y) $ sort l
--       in map (\e@((k, _) : _) -> (k, sum $ map snd e)) g

-- Test list for factorize
testList = Prob [(1, 1%4), (2, 2%4), (1, 1%4)]

instance Monad Prob where
    return x = Prob [(x, 1)]
    (Prob l) >>= f = undefined

sameProbability :: [a] -> Prob a
sameProbability l = Prob $ map (\x -> (x, 1  % (length l))) l

dice = sameProbability [1..6]
