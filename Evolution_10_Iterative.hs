-- Iterative Haskell programmer
-- (former Pascal programmer)

module Evolution_10_Iterative where

import Prelude hiding (init)

fac :: Integer -> Integer

fac n = result (for init next done)
        where init = (0,1)
              next   (i,m) = (i+1, m * (i+1))
              done   (i,_) = i==n
              result (_,m) = m

for :: a -> (a -> a) -> (a -> Bool) -> a
for i n d = until d n i
