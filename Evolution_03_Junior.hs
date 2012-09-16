-- Junior Haskell programmer
-- (beginning Peano player)

module Evolution_03_Junior where

fac :: Integer -> Integer
fac 0    =  1
fac n = n * fac (n-1)
