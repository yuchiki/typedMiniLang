module Eval(eval, eval', Env) where

import qualified Data.Map as Map
import Variable
import Expr
import Value


type Env = Map.Map Variable Value

expectInt :: Value -> Maybe Int
expectInt (VInt i) = Just i
expectInt _ = Nothing

expectBool :: Value -> Maybe Bool
expectBool (VBool b) = Just b
expectBool _ = Nothing

expectAbs :: Value -> Maybe (Variable, Expr)
expectAbs (VAbs x e) = Just (x, e)
expectAbs _ = Nothing

type BinOp = Env -> Expr -> Expr -> Maybe Value

type BinOpGenerator a b = (a -> a -> b) -> BinOp

binOp :: (Value -> Maybe a) -> (b -> Maybe Value) -> BinOpGenerator a b
binOp from to op env e1 e2 = do
    v1 <- eval' env e1 >>= from
    v2 <- eval' env e2 >>= from
    to $ op v1 v2

intBinOp :: BinOpGenerator Int Int
intBinOp = binOp expectInt (return . VInt)

boolBinOp :: BinOpGenerator Bool Bool
boolBinOp = binOp expectBool (return . VBool)

compBinOp :: BinOpGenerator Int Bool
compBinOp = binOp expectInt (return . VBool)

eval :: Expr -> Maybe Value
eval = eval' Map.empty

eval' :: Env -> Expr -> Maybe Value
eval' env (EVar v) = Map.lookup v env
eval' env (EBool b) = Just $ VBool b
eval' env (EInt i) = Just $ VInt i
eval' env (EAdd e1 e2) = intBinOp (+) env e1 e2
eval' env (ESub e1 e2) = intBinOp (-) env e1 e2
eval' env (EMul e1 e2) = intBinOp (*) env e1 e2
eval' env (EDiv e1 e2) = intBinOp div env e1 e2
eval' env (EAnd e1 e2) = boolBinOp (&&) env e1 e2
eval' env (EOr e1 e2) = boolBinOp (||) env e1 e2
eval' env (ENot e) = VBool . not <$> (eval' env e >>= expectBool)
eval' env (ELT e1 e2) = compBinOp (<) env e1 e2
eval' env (EEqInt e1 e2) = compBinOp (==) env e1 e2
eval' env (EIf e1 e2 e3) = do
    p <- eval' env e1 >>= expectBool
    v1 <- eval' env e2
    v2 <- eval' env e3
    return $ if p then v1 else v2
eval' env (ELet x e1 e2) = do
    v1 <- eval' env e1
    let newEnv = Map.insert x v1 env
    eval' newEnv e2
eval' env (EAbs x e) = return $ VAbs x e --TODO: FIX: 間違っている。よく見るとclosureをもっていない
eval' env (EApp e1 e2) = do
    (x, e) <- eval' env e1 >>= expectAbs
    v2 <- eval' env e2
    let newEnv = Map.insert x v2 env
    eval' newEnv e
eval' env ENil = Just VNil
eval' env (ECons e1 e2) = do
    v1 <- eval' env e1
    v2 <- eval' env e2
    Just $ VCons v1 v2
eval' env (EMatch e1 e2 x1 x2 e3) = do
    v1 <- eval' env e1
    case v1 of
        VNil -> eval' env e2
        VCons vhead vtail -> eval' (Map.insert x1 vhead $ Map.insert x2 vtail env) e3
