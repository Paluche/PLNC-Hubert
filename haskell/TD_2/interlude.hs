module Interlude where

import GHC.List

cleave :: a -> [a -> b] -> [b]
cleave x (f : []) = [f x]
cleave x (f : fs) = f x : cleave x fs

spread :: [a -> b] -> [a] -> [b]
spread (f : []) (x : xs) = [f x]
spread (f : fs) (x : []) = [f x]
spread (f : fs) (x : xs) = f x : spread fs xs

testF = [(<1), (<2), (<3), (<4)]

testL = [3, 2, 1]
