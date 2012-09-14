-- List-encoding Haskell programmer
-- (prefers to count in unary)

module Evolution_16_ListEncoding where

arb = ()    -- "undefined" is also a good RHS, as is "arb" :)

listenc n = replicate n arb
listprj f = length . f . listenc

listprod xs ys = [ i (x,y) | x<-xs, y<-ys ]
                 where i _ = arb

facl []         = listenc  1
facl n@(_:pred) = listprod n (facl pred)

fac = listprj facl
