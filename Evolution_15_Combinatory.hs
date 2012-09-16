-- Combinatory Haskell programmer
-- (eschews variables, if not obfuscation;
-- all this currying’s just a phase, though it seldom hinders)

module Evolution_15_Combinatory where

s f g x = f x (g x)

k x y   = x

b f g x = f (g x)

c f g x = f x g

y f     = f (y f)

cond p f g x = if p x then f x else g x

fac :: Integer -> Integer
fac  = y (b (cond ((==) 0) (k 1)) (b (s (*)) (c b pred)))
