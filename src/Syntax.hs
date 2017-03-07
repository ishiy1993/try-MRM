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

class Trans fs where
    trAlg :: Matches fs Exp Exp

instance Trans ('[]) where
    trAlg = Void

instance Trans fs => Trans (Val ': fs) where
    trAlg = trVal ::: trAlg

instance Trans fs => Trans (Add ': fs) where
    trAlg = trAdd ::: trAlg

instance Trans fs => Trans (Mul ': fs) where
    trAlg = trMul ::: trAlg

trans :: Trans fs => Fix fs -> Exp
trans = fold trAlg

trVal :: Val Exp -> Exp
trVal (N n) = val n

trAdd :: Add Exp -> Exp
trAdd (Add e1 e2) = add (trans e1) (trans e2)

trMul :: Mul Exp -> Exp
trMul (Mul e1 e2) 
    | isVal e1 && isVal e2 = if isZero e2
                                then val 0
                                else add e1 (trans $ mul e1 (sub1 e2))
    | isVal e1 && isAdd e2 = let Just (Add e21 e22) = prj e2
                             in  add (trans $ mul e1 e21)
                                     (trans $ mul e1 e22)
    | isAdd e1 && isVal e2 = let Just (Add e11 e12) = prj e1
                             in  add (trans $ mul e11 e2)
                                     (trans $ mul e12 e2)
    | isAdd e1 && isAdd e2 = let Just (Add e11 e12) = prj e1
                                 Just (Add e21 e22) = prj e2
                             in  add (add (trans $ mul e11 e21)
                                          (trans $ mul e11 e22))
                                     (add (trans $ mul e12 e21)
                                          (trans $ mul e12 e22))
    | otherwise = trans $ mul (trans e1) (trans e2)

pattern NumP n <- (prj -> Just (N n))

sub1 :: Exp -> Exp
sub1 (NumP n) = if n > 0 then val (n-1) else val (n+1)

isZero :: Exp -> Bool
isZero (NumP n) = n == 0

isVal :: Exp -> Bool
isVal = match $ (\(N _) -> True)
            ::: (\(Add _ _) -> False)
            ::: (\(Mul _ _) -> False)
            ::: Void

isAdd :: Exp -> Bool
isAdd = match $ (\(N _) -> False)
            ::: (\(Add _ _) -> True)
            ::: (\(Mul _ _) ->  False)
            ::: Void

-- 必要なもの: 関数
-- f :: Mul (Fix '[Val, Add]) -> Fix '[Val, Add]
-- これを実装するにはかけ算の非再帰的定義が必要
--
-- これがあれば、
-- desugarAlg = f ::: transAlg

-- desugarAlg :: (gs <: gs, Mem Add gs, Mem Val gs)
--            => Algebras (Mul ': gs) (Fix gs)
-- desugarAlg = (\(Mul e1 e2) -> fold (reduceAlg e1) e2) ::: transAlg

-- desugar :: (fs <: fs, Mem Add fs, Mem Val fs)
--         => Fix (Mul ': fs) -> Fix fs
-- desugar = fold desugarAlg


-- class Reduce gs where
--     reduceAlg :: Fix gs -> Algebras gs (Fix gs)
    -- reduceAlg :: (gs <: gs, Mem Add gs, Mem Val gs)
    --           => Fix gs -> Algebras gs (Fix gs)

-- instance Reduce ('[]) where
--     reduceAlg _ = Void

-- instance Reduce fs => Reduce (Val ': fs) where
--     reduceAlg m = reduceVal m ::: reduceAlg m

-- instance Reduce fs => Reduce (Add ': fs) where
--     reduceAlg m = reduceAdd m ::: reduceAlg m

-- reduceVal :: (fs <: fs, Mem Add fs, Mem Val fs)
--           => Fix fs -> Val (Fix fs) -> Fix fs
-- reduceVal m (N n) = add m (val n)

-- reduceAdd :: (fs <: fs, Mem Add fs, Mem Val fs)
--           => Fix fs -> Add (Fix fs) -> Fix fs
-- reduceAdd m (Add n1 n2) = add m (add n1 n2)
