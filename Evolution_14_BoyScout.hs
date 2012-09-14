-- Boy Scout Haskell programmer
-- (likes tying knots; always “reverent,” he
-- belongs to the Church of the Least Fixed-Point [8])

module Evolution_14_BoyScout where

y f = f (y f)

fac = y (\f n -> if (n==0) then 1 else n * f (n-1))
