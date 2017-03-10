{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module PrettyShow where

import Lib
import Syntax

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
