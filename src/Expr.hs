module Expr where

type Variable = String

data Expr =
    EVar Variable
    | ECBool Bool
    | ECInt Int
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
    deriving (Show)
