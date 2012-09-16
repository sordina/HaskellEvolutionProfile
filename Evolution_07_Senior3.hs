-- Yet another senior Haskell programmer
-- (leaned so far right he came back left again!)
-- using foldr to simulate foldl

module Evolution_07_Senior3 where

fac :: Integer -> Integer
fac n' = foldr (\x g n -> g (x*n)) id [1..n'] 1
