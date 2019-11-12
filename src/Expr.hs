module Expr where

import Variable

data Expr =
    EVar Variable
    | EBool Bool
    | EInt Int
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | ENot Expr
    | ELT Expr Expr
    | EEqInt Expr Expr
    | EIf Expr Expr Expr
    | ELet Variable Expr Expr
    | EAbs Variable Expr
    | EApp Expr Expr
    | ENil
    | ECons Expr Expr
    | EMatch Expr Expr Variable Variable Expr
    deriving (Show, Eq)
