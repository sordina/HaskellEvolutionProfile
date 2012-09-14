-- Origamist Haskell programmer
-- (always starts out with the “basic Bird fold”)
-- (curried, list) fold and an application

module Evolution_20_Origamist where

fold c n []     = n
fold c n (x:xs) = c x (fold c n xs)

prod = fold (*) 1


-- (curried, boolean-based, list) unfold and an application

unfold p f g x =
  if p x
     then []
     else f x : unfold p f g (g x)

downfrom = unfold (==0) id pred


-- hylomorphisms, as-is or "unfolded" (ouch! sorry ...)

refold  c n p f g   = fold c n . unfold p f g

refold' c n p f g x =
  if p x
     then n
     else c (f x) (refold' c n p f g (g x))


-- several versions of factorial, all (extensionally) equivalent

fac   = prod . downfrom
fac'  = refold  (*) 1 (==0) id pred
fac'' = refold' (*) 1 (==0) id pred
