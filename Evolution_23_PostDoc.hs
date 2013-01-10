-- Post-doc Haskell programmer
-- (from Uustalu, Vene and Pardo’s “Recursion Schemes from Comonads” [4])
-- explicit type recursion with functors and catamorphisms

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Evolution_23_PostDoc where

import Control.DeepSeq (NFData(rnf))
import Data.List (genericTake)

newtype Mu f = In (f (Mu f))

unIn (In x) = x

cata phi = phi . fmap (cata phi) . unIn


-- base functor and data type for natural numbers,
-- using locally-defined "eliminators"

data N c = Z | S c

instance Functor N where
  fmap g  Z    = Z
  fmap g (S x) = S (g x)

type Nat = Mu N

instance NFData Nat where
   rnf (In (S x)) = rnf x `seq` ()
   rnf (In Z)     = ()
instance Num Nat where
   (+) = add
   (*) = mult
   signum (In Z) = zero
   signum _      = suck zero
   fromInteger n | n >= 0 = last . genericTake (n+1) . iterate suck $ zero
                 | n <  0 = negate . fromInteger . abs $ n
   abs = error "Nat is not negative"

zero   = In  Z
suck n = In (S n)

add m = cata phi where
  phi  Z    = m
  phi (S f) = suck f

mult m = cata phi where
  phi  Z    = zero
  phi (S f) = add m f


-- explicit products and their functorial action

data Prod e c = Pair c e

outl (Pair x y) = x
outr (Pair x y) = y

fork f g x = Pair (f x) (g x)

instance Functor (Prod e) where
  fmap g = fork (g . outl) outr


-- comonads, the categorical "opposite" of monads

class Functor n => Comonad n where
  extr :: n a -> a
  dupl :: n a -> n (n a)

instance Comonad (Prod e) where
  extr = outl
  dupl = fork id outr


-- generalized catamorphisms, zygomorphisms and paramorphisms

gcata :: (Functor f, Comonad n) =>
           (forall a. f (n a) -> n (f a))
             -> (f (n c) -> c) -> Mu f -> c

gcata dist phi = extr . cata (fmap phi . dist . fmap dupl)

zygo chi = gcata (fork (fmap outl) (chi . fmap outr))

para :: Functor f => (f (Prod (Mu f) c) -> c) -> Mu f -> c
para = zygo In


-- factorial, the *hard* way!

fac = para phi where
  phi  Z             = suck zero
  phi (S (Pair f n)) = mult f (suck n)


-- for convenience and testing

int = cata phi where
  phi  Z    = 0
  phi (S f) = 1 + f

instance Show (Mu N) where
  show = show . int
