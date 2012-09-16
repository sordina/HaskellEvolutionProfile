-- Iterative one-liner Haskell programmer
-- (former APL and C programmer)

module Evolution_11_IterativeOneLiner where

fac :: Integer -> Integer
fac n = snd (until ((>n) . fst) (\(i,m) -> (i+1, i*m)) (1,1))
