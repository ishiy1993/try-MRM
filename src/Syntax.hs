{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Syntax where

import Lib

data Val x = N Int deriving Functor
data Add x = Add x x deriving Functor
data Mul x = Mul x x deriving Functor

deriving instance Show x => Show (Val x)
deriving instance Show x => Show (Add x)
deriving instance Show x => Show (Mul x)

type Exp = Fix '[Val, Add, Mul]

val :: Mem Val fs => Int -> Fix fs
val = inn . N

add :: Mem Add fs => Fix fs -> Fix fs -> Fix fs
add x y = inn $ Add x y

mul :: Mem Mul fs => Fix fs -> Fix fs -> Fix fs
mul x y = inn $ Mul x y

exp1 :: Exp
exp1 = add (mul (val 2) (val 3)) (add (val 2) (val 2))

exp2 :: Exp
exp2 = mul (val 3) (add (val 2) (val 4))

