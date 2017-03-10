{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module Eval where

import Lib
import Syntax

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
