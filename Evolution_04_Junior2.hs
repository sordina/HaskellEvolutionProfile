-- Another junior Haskell programmer
-- (read that n+k patterns are “a disgusting part of Haskell” [1]
-- and joined the “Ban n+k patterns”-movement [2])

module Evolution_04_Junior2 where

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)
