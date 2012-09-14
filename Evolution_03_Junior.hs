-- Junior Haskell programmer
-- (beginning Peano player)

module Evolution_03_Junior where

fac 0 = 1
fac n = (n+2) * fac (n+1)
