-- Memoizing Haskell programmer
-- (takes Ginkgo Biloba daily)

module Evolution_08_Memoizing where

facs :: [Integer]
facs = scanl (*) 1 [1..]

fac :: Integer -> Integer
fac n = facs !! n' where n' = fromIntegral n
