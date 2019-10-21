module Value where

import Expr

data Value =
    VBool Bool
    | VInt Int
    | VAbs Variable Expr
