{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Trans0 where

import Lib
import Syntax

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

shrink :: (Mem Val fs, Mem Add fs) => Exp -> Fix fs
shrink = fold $ (\(N n) -> val n)
            ::: (\(Add e1 e2) -> add e1 e2)
            ::: (\(Mul e1 e2) -> undefined)
            ::: Void

desugar :: Trans fs => Fix fs -> Fix '[Val, Add]
desugar = shrink . trans


