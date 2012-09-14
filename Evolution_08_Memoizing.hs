-- Memoizing Haskell programmer
-- (takes Ginkgo Biloba daily)

module Evolution_08_Memoizing where

facs = scanl (*) 1 [1..]

fac n = facs !! n
