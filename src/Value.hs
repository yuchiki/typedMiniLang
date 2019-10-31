module Value where

import Expr

data Value =
    VBool Bool
    | VInt Int
    | VAbs Variable Expr
    | VNil
    | VCons Value Value
    deriving (Show, Eq)
