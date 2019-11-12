{-# LANGUAGE TemplateHaskell #-}

module SimpleTyping where

import qualified Data.Map as Map
import Development.Placeholders
import SimpleType
import Expr

type TypeEquation = (SimpleType, SimpleType)

extract :: TypeIDGenerator -> TypeEnv -> Expr -> Maybe (TypeIDGenerator , [TypeEquation], SimpleType)
extract gen tenv (EVar v) = do
    t <- Map.lookup v tenv
    return (gen, [], t)
extract gen tenv (EBool _) = return (gen, [], TBool)
extract gen tenv (EInt _) = return (gen, [], TInt)
extract gen tenv (EAdd e1 e2) = extractIntOp gen tenv e1 e2
extract gen tenv (ESub e1 e2) = extractIntOp gen tenv e1 e2
extract gen tenv (EMul e1 e2) = extractIntOp gen tenv e1 e2
extract gen tenv (EDiv e1 e2) = extractIntOp gen tenv e1 e2
extract gen tenv (EAnd e1 e2) = extractBoolOp gen tenv e1 e2
extract gen tenv (EOr e1 e2) = extractBoolOp gen tenv e1 e2
extract gen tenv (ENot e) = do
    (gen1, eqs, t) <- extract gen tenv e
    return (gen1, (t, TBool) : eqs, TBool)
extract gen tenv (ELT e1 e2) = extractCompOp gen tenv e1 e2
extract gen tenv (EEqInt e1 e2) = extractCompOp gen tenv e1 e2
extract gen tenv (EIf e1 e2 e3) = do
    (gen1, eqs1, t1) <- extract gen tenv e1
    (gen2, eqs2, t2) <- extract gen1 tenv e2
    (gen3, eqs3, t3) <- extract gen2 tenv e3
    let eqs = (t1, TBool) : (t2, t3) : eqs1 ++ eqs2 ++ eqs3
    return (gen3, eqs, t2)
extract gen tenv (ELet x e1 e2) = do
    (gen1, eqs1, t1) <- extract gen tenv e1
    let newTEnv = Map.insert x t1 tenv
    (gen2, eqs2, t2) <- extract gen1 newTEnv e2
    return (gen2, eqs1 ++ eqs2, t2)
extract gen tenv (EAbs x e) = do
    let (tv, gen') = genFreshTVar gen
    let newTEnv = Map.insert x tv tenv
    (gen1, eqs1, t1) <- extract gen' newTEnv e
    return (gen1, eqs1, TFun tv t1)
extract gen tenv (EApp e1 e2) = do
    (gen1, eqs1, t1) <- extract gen tenv e1
    (gen2, eqs2, t2) <- extract gen1 tenv e2
    let (tv, gen') = genFreshTVar gen2
    let eqs = (t1, TFun t2 tv) : eqs1 ++ eqs2
    return (gen', eqs, tv)
extract gen tenv ENil = $notImplemented -- リストはまだサポートしない
extract gen tenv (ECons e1 e2) = $notImplemented -- リストはまだサポートしない
extract gen tenv (EMatch e1 e2 x1 x2 e3) = $notImplemented -- リストはまだサポートしない


extractIntOp :: TypeIDGenerator -> TypeEnv -> Expr -> Expr -> Maybe (TypeIDGenerator, [TypeEquation], SimpleType)
extractIntOp gen tenv e1 e2 = do
    (gen1, eqs1, t1) <- extract gen tenv e1
    (gen2, eqs2, t2) <- extract gen1 tenv e2
    return (gen2, (t1, TInt) : (t2, TInt) : eqs1 ++ eqs2, TInt)

extractBoolOp :: TypeIDGenerator -> TypeEnv -> Expr -> Expr -> Maybe (TypeIDGenerator, [TypeEquation], SimpleType)
extractBoolOp gen tenv e1 e2 = do
    (gen1, eqs1, t1) <- extract gen tenv e1
    (gen2, eqs2, t2) <- extract gen1 tenv e2
    return (gen2, (t1, TBool) : (t2, TBool) : eqs1 ++ eqs2, TBool)

extractCompOp :: TypeIDGenerator -> TypeEnv -> Expr -> Expr -> Maybe (TypeIDGenerator, [TypeEquation], SimpleType)
extractCompOp gen tenv e1 e2 = do
    (gen1, eqs1, t1) <- extract gen tenv e1
    (gen2, eqs2, t2) <- extract gen1 tenv e2
    return (gen2, (t1, TInt) : (t2, TInt) : eqs1 ++ eqs2, TBool)
