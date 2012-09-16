-- Accumulating Haskell programmer
-- (building up to a quick climax)

module Evolution_12_Accumulating where

facAcc :: (Eq a, Num a) => a -> a -> a
facAcc a 0 = a
facAcc a n = facAcc (n*a) (n-1)

fac :: Integer -> Integer
fac = facAcc 1
