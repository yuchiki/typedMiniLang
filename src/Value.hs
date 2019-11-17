module Value where

import Expr
import Variable

data Value =
    VBool Bool
    | VInt Int
    | VAbs Variable Expr
    | VNil
    | VCons Value Value
    deriving (Eq)

instance Show Value where
    show (VBool True) = "true"
    show (VBool False) = "false"
    show (VInt i) = show i
    show (VAbs v e) = "fun " ++ v ++ " -> " ++ show e
    show VNil = "[]"
    show (VCons e1 e2) = "(" ++  show e1 ++ "):(" ++ show e2 ++ ")"
