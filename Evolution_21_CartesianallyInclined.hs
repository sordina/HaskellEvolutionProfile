-- Cartesianally-inclined Haskell programmer
-- (prefers Greek food, avoids the spicy Indian stuff;
-- inspired by Lex Augusteijn’s “Sorting Morphisms” [3])
-- (product-based, list) catamorphisms and an application

module Evolution_21_CartesianallyInclined where

cata (n,c) []     = n
cata (n,c) (x:xs) = c (x, cata (n,c) xs)

mult = uncurry (*)
prod = cata (1, mult)


-- (co-product-based, list) anamorphisms and an application

ana f = either (const []) (cons . pair (id, ana f)) . f

cons = uncurry (:)

downfrom = ana uncount

uncount 0 = Left  ()
uncount n = Right (n, n-1)


-- two variations on list hylomorphisms

hylo  f  g    = cata g . ana f

hylo' f (n,c) = either (const n) (c . pair (id, hylo' f (n,c))) . f

pair (f,g) (x,y) = (f x, g y)


-- several versions of factorial, all (extensionally) equivalent

fac   = prod . downfrom
fac'  = hylo  uncount (1, mult)
fac'' = hylo' uncount (1, mult)
