{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Lib where

data Fix (fs :: [* -> *]) where
    In :: Functor f => Elem  f fs -> f (Fix fs) -> Fix fs

inn :: (Mem f fs, Functor f) => f (Fix fs) -> Fix fs
inn = In witness

prj :: (fs <: fs, Mem f fs) => Fix fs -> Maybe (f (Fix fs))
prj = match $ Just >:: fromFunction (\pos -> const Nothing)

data Elem (f :: * -> *) (fs :: [* -> *]) where
    Here :: Elem f (f ': fs)
    There :: Elem f fs -> Elem f (g ': fs)

class Mem f fs where
    witness :: Elem f fs

instance {-# OVERLAPS #-} Mem f (f ': fs) where
    witness = Here

instance (Mem f fs) => Mem f (g ': fs) where
    witness = There witness

data Matches (fs :: [* -> *]) (a :: *) (b :: *) where
    Void :: Matches '[] a b
    (:::) :: Functor f => (f a -> b) -> Matches fs a b -> Matches (f ': fs) a b

infixr 6 :::

extractAt :: Elem f fs -> Matches fs a b -> (f a -> b)
extractAt Here (f:::_) = f
extractAt (There pos) (_:::fs) = extractAt pos fs

match :: Matches fs (Fix fs) b -> Fix fs -> b
match fs (In pos xs) = extractAt pos fs xs

type Algebras fs a = Matches fs a a

fold :: Algebras fs a -> Fix fs -> a
fold ks (In pos xs) = extractAt pos ks (fmap (fold ks) xs)

(<<^) :: Matches fs b c -> (a -> b) -> Matches fs a c
Void <<^ g = Void
(h ::: hs) <<^ g = (h . fmap g) ::: (hs <<^ g)

(^<<) :: (b -> c) -> Matches fs a b -> Matches fs a c
g ^<< Void = Void
g ^<< (h ::: hs) = (g . h) ::: (g ^<< hs)

(&&&) :: Matches fs a b -> Matches fs a c -> Matches fs a (b,c)
Void &&& Void = Void
(h ::: hs) &&& (k ::: ks) = (\x -> (h x, k x)) ::: (hs &&& ks)

infixr 7 <<^, ^<<
infixr 5 &&&

para :: (fs <: fs) => Matches fs (a, Fix fs) a -> Fix fs -> a
para ks = fst . fold (ks &&& (inns <<^ snd))

inns :: (fs <: fs) => Algebras fs (Fix fs)
inns = transAlg

-- Subtyping
data Sub (fs :: [* -> *]) (gs :: [* -> *]) where
    SNil :: Sub '[] gs
    SCons :: (Functor f) => Elem f gs -> Sub fs' gs -> Sub (f ': fs') gs

class fs <: gs where
    srep :: Sub fs gs

instance '[] <: gs where
    srep = SNil

instance (Functor f, Mem f gs, fs <: gs) => (f ': fs) <: gs where
    srep = SCons witness srep

infix 5 <:

subFix :: (fs <: gs) => Fix fs -> Fix gs
subFix = fold transAlg

transAlg :: (fs <: gs) => Algebras fs (Fix gs)
transAlg = transAlg' srep

transAlg' :: Sub fs gs -> Algebras fs (Fix gs)
transAlg' SNil = Void
transAlg' (SCons pos xs) = In pos ::: transAlg' xs

subOp :: (fs <: gs) => (Fix gs -> c) -> Fix fs -> c
subOp f = f . subFix

subMatch :: (fs <: gs) => Matches gs r a -> Matches fs r a
subMatch = subMatch' srep

subMatch' :: Sub fs gs -> Matches gs r a -> Matches fs r a
subMatch' SNil _ = Void
subMatch' (SCons pos xs) as = extractAt pos as ::: subMatch' xs as

overrideAt :: Elem f fs -> (f a -> b) -> Matches fs a b -> Matches fs a b
overrideAt Here g (_ ::: fs) = g ::: fs
overrideAt (There pos) g (f ::: fs) = f ::: overrideAt pos g fs

(>::) :: Mem f fs => (f a -> b) -> Matches fs a b -> Matches fs a b
(>::) = overrideAt witness

infixr 6 >::

fromFunctionWith :: (forall f. Functor f => Elem f fs -> f a -> b)
                 -> Sub gs fs -> Matches gs a b
fromFunctionWith f SNil = Void
fromFunctionWith f (SCons pos ss) = f pos ::: fromFunctionWith f ss

fromFunction :: (fs <: fs) => (forall f. Functor f => Elem f fs -> f a -> b) -> Matches fs a b
fromFunction f = fromFunctionWith f srep

