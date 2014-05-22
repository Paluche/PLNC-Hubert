module Interlude where

import GHC.List

-- Iterative obvious solution
cleave :: a -> [a -> b] -> [b]
cleave _ [] = []
cleave x (f : fs) = f x : cleave x fs

spread :: [a -> b] -> [a] -> [b]
spread [] _ = []
spread _ [] = []
spread (f : fs) (x : xs) = f x : spread fs xs

-- A much better interesting solution which definition is on one line
-- I think this is clever but the other definition was shorter. Bad idea.
cleave' :: a -> [a -> b] -> [b]
cleave' x f = map (\a -> (f!!a) x) [0 .. length f - 1]

-- With the same idea
spread' :: [a -> b] -> [a] -> [b]
spread' f x = map (\a -> (f!!a) (x!!a)) [0 .. (min (length f) $ (length x)) - 1]

-- Value Test
testF = [(<1), (<2), (<3), (<4)]
testL = [   4,   3,   2,   1]

