-- Freshman Haskell programmer

module Evolution_01_Freshman where

fac :: Integer -> Integer
fac n = if n == 0
           then 1
           else n * fac (n-1)
