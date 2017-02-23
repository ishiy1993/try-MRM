{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
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

class Eval fs where
    evalAlg :: Matches fs Int Int

instance Eval ('[]) where
    evalAlg = Void

instance Eval fs => Eval (Val ': fs) where
    evalAlg = evVal ::: evalAlg

instance Eval fs => Eval (Add ': fs) where
    evalAlg = evAdd ::: evalAlg

instance Eval fs => Eval (Mul ': fs) where
    evalAlg = evMul ::: evalAlg

eval :: Eval fs => Fix fs -> Int
eval = fold evalAlg

evVal :: Val Int -> Int
evVal (N n) = n

evAdd :: Add Int -> Int
evAdd (Add n1 n2) = n1 + n2

evMul :: Mul Int -> Int
evMul (Mul n1 n2) = n1 * n2

class PrettyShow fs where
    showAlg :: Matches fs String String

instance PrettyShow ('[]) where
    showAlg = Void

instance PrettyShow fs => PrettyShow (Val ': fs) where
    showAlg = showVal ::: showAlg

instance PrettyShow fs => PrettyShow (Add ': fs) where
    showAlg = showAdd ::: showAlg

instance PrettyShow fs => PrettyShow (Mul ': fs) where
    showAlg = showMul ::: showAlg

pshow :: PrettyShow fs => Fix fs -> String
pshow = fold showAlg

showVal :: Val String -> String
showVal (N n) = show n

showAdd :: Add String -> String
showAdd (Add n1 n2) = "(" ++ n1 ++ " + " ++ n2 ++ ")"

showMul :: Mul String -> String
showMul (Mul n1 n2) = "(" ++ n1 ++ " * " ++ n2 ++ ")"

