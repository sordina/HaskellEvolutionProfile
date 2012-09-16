-- Senior Haskell programmer
-- (voted for   Nixon   Buchanan   Bush — “leans right”)

module Evolution_05_Senior where

fac :: Integer -> Integer
fac n = foldr (*) 1 [1..n]
