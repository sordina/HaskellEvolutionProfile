-- Continuation-passing Haskell programmer
-- (raised RABBITS in early years, then moved to New Jersey)

module Evolution_13_ContinuationPassing where

facCps :: (Eq b, Num b) => (b -> c) -> b -> c
facCps k 0 = k 1
facCps k n = facCps (k . (n *)) (n-1)

fac :: Integer -> Integer
fac = facCps id
