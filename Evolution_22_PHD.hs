-- Ph.D. Haskell programmer
-- (ate so many bananas that his eyes bugged out, now he needs new lenses!)
-- explicit type recursion based on functors

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Evolution_22_PHD where

newtype Mu f = Mu (f (Mu f)) 

instance Show (f (Mu f)) where
  show x = "HELLO"

in'      x  = Mu x
out (Mu x) = x


-- cata- and ana-morphisms, now for *arbitrary* (regular) base functors

cata phi = phi . fmap (cata phi) . out
ana  psi = in'  . fmap (ana  psi) . psi


-- base functor and data type for natural numbers,
-- using a curried elimination operator

data N b = Zero | Succ b  deriving Show

instance Functor N where
  fmap f = nelim Zero (Succ . f)

nelim z s  Zero    = z
nelim z s (Succ n) = s n

type Nat = Mu N


-- conversion to internal numbers, conveniences and applications

int = cata (nelim 0 (1+))

instance Show Nat where
  show = show . int

zero = in'   Zero
suck = in' . Succ       -- pardon my "French" (Prelude conflict)

plus n = cata (nelim n     suck   )
mult n = cata (nelim zero (plus n))


-- base functor and data type for lists

data L a b = Nil | Cons a b  deriving Show

instance Functor (L a) where
  fmap f = lelim Nil (\a b -> Cons a (f b))

lelim n c  Nil       = n
lelim n c (Cons a b) = c a b

type List a = Mu (L a)


-- conversion to internal lists, conveniences and applications

list = cata (lelim [] (:))

instance Show a => Show (List a) where
  show = show . list

prod = cata (lelim (suck zero) mult)

upto = ana (nelim Nil (diag (Cons . suck)) . out)

diag f x = f x x

fac = prod . upto
