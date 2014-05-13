-- Peano numbers

import Prelude

data Peano = Zero | Succ Peano

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
peano_fromInteger  a = Succ $ peano_fromInteger (a - 1)

instance Num Peano where
    a + b         = peano_add a b
    a - b         = peano_sub a b
    a * b         = peano_mul a b
    signum a      = peano_sign a
    abs a         = a
    fromInteger a = peano_fromInteger a
