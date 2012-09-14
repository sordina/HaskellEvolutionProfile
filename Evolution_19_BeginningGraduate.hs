-- Beginning graduate Haskell programmer
-- (graduate education tends to liberate one from petty concerns
-- about, e.g., the efficiency of hardware-based integers)
-- the natural numbers, a la Peano

module Evolution_19_BeginningGraduate where

data Nat = Zero | Succ Nat


-- iteration and some applications

iter z s  Zero    = z
iter z s (Succ n) = s (iter z s n)

plus n = iter n     Succ
mult n = iter Zero (plus n)


-- primitive recursion

primrec z s  Zero    = z
primrec z s (Succ n) = s n (primrec z s n)


-- two versions of factorial

fac  = snd . iter (one, one) (\(a,b) -> (Succ a, mult a b))
fac' = primrec one (mult . Succ)


-- for convenience and testing (try e.g. "fac five")

int = iter 0 (1+)

instance Show Nat where
  show = show . int

(zero : one : two : three : four : five : _) = iterate Succ Zero
