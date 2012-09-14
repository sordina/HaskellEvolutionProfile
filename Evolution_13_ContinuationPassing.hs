-- Continuation-passing Haskell programmer
-- (raised RABBITS in early years, then moved to New Jersey)

module Evolution_13_ContinuationPassing where

facCps k 0 = k 1
facCps k n = facCps (k . (n *)) (n-1)

fac = facCps id
