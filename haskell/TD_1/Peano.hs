-- Peano numbers

module Peano where

import Prelude

-- Peano definition
data Peano = Zero | Succ Peano

-- Instance of Num peano
peano_add :: Peano -> Peano -> Peano
peano_add Zero b        = b
peano_add (Succ a) b    = Succ (peano_add a b)

peano_sub :: Peano -> Peano -> Peano
peano_sub b Zero            = b
peano_sub (Succ a) (Succ b) = peano_sub a b

peano_mul :: Peano -> Peano -> Peano
peano_mul Zero b     = Zero
peano_mul a (Succ b) = peano_add a $ peano_mul a b

peano_sign :: Peano -> Peano
peano_sign Zero = Zero
peano_sign a    = Succ Zero

peano_fromInteger :: Integer -> Peano
peano_fromInteger  0 = Zero
peano_fromInteger  a | a < 0     = error "Peano numbers are positive"
                     | otherwise = Succ $ peano_fromInteger (a - 1)
instance Num Peano where
    a + b         = peano_add a b
    a - b         = peano_sub a b
    a * b         = peano_mul a b
    signum a      = peano_sign a
    abs a         = a
    fromInteger a = peano_fromInteger a


-- Instance of Eq class
instance Eq Peano where
    -- TODO

-- Instance of Ord class
peano_less :: Peano -> Peano -> Bool
peano_less a Zero            = False
peano_less Zero (Succ a)     = True
peano_less (Succ a) (Succ b) = peano_less a b

instance Ord Peano where
    (<) = peano_less

-- Instance of Enum class
-- TODO toSomething and fromSomething function are the same, can't I have one
-- function? I tried but didn"t succeed.
peano_toEnum :: Int -> Peano
peano_toEnum 0  = Zero
peano_toEnum a = Succ (peano_toEnum a)

peano_fromEnum :: Peano -> Int
peano_fromEnum Zero     = 0
peano_fromEnum (Succ a) = 1 + peano_fromEnum a

instance Enum Peano where
    toEnum = peano_toEnum
    fromEnum = peano_fromEnum


-- Instance of Real class
peano_toRational :: Peano -> Rational
peano_toRational Zero = 0
peano_toRational (Succ a) = 1 + peano_toRational a

instance Real Peano where
    toRational = peano_toRational


-- Instance of integral class
peano_div :: Peano -> Peano -> Peano
peano_div _ Zero = error "Division by zero"
peano_div a b | a < b = Zero
              | a == b = Succ Zero
              | otherwise = Succ $ peano_div (a - b) b

peano_toInteger :: Peano -> Integer
peano_toInteger Zero     = 0
peano_toInteger (Succ a) = 1 + peano_toInteger a

peano_quotRem :: Peano -> Peano -> (Peano, Peano)
peano_quotRem a b = (result, a - result)
                    where result = peano_div a b

instance Integral Peano where
    div        = peano_div
    toInteger  = peano_toInteger
    quotRem    = peano_quotRem


-- Instance of Show class
peano_show :: Peano -> String
peano_show Zero     = "Zero"
peano_show (Succ a) = "Succ " ++ peano_show a

instance Show Peano where
    show = peano_show


-- Instance of Read class

-- I guess what they want is an infinite list of couple i
-- (value, equivalent String)
-- To a given String we return the list of possible couple that match the
-- String.
-- I though about  parsing the string in words, when a word is "Succ" you ++
-- with the next element until we found the "Zero" otherwise the String is not
-- representing a Peano number.
-- Now we have list of Peano numbers in String that we can compare with the
-- list of solution.

-- I didn't succeed to do this function and I just feel like I've complicated
-- this too much and probably a simpler and eleganter solution exists.
--  or I didn't get what they are expecting.

stringPeano :: Int -> String
stringPeano 0 = "Zero"
stringPeano a = "Succ " ++ stringPeano (a - 1)

couplePeano :: Int -> (Peano, String)
couplePeano a = (peano_fromInteger a, stringPeano a)

listPeano = (map couplePeano) . [1..]

readPeano :: ReadS Peano -- String -> [(Peano, String)]
readPeano

peano_readsPrec :: Int -> ReadS Peano
peano_readsPrec _ =

instance Read Peano where
    readsPrec = peano_readsPrec
