-- Another senior Haskell programmer
-- (voted for   McGovern   Biafra   Nader — “leans left”)

module Evolution_06_Senior2 where

fac :: Integer -> Integer
fac n = foldl (*) 1 [1..n]
