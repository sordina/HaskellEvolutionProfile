-- Sophomore Haskell programmer, at MIT
-- (studied Scheme as a freshman)

module Evolution_02_Sophomore where

fac :: Integer -> Integer
fac = (\(n) ->
        (if ((==) n 0)
            then 1
            else ((*) n (fac ((-) n 1)))))
