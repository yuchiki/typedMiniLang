{-# LANGUAGE TemplateHaskell #-}

module SimpleTyping(infer, extract, unify, substitute, TypeEquation, showTypeEquations, Substitution, showSubstitution, InferResult(..)) where

import qualified Data.Map as Map
import Development.Placeholders
import SimpleType
import Expr

type TypeEquation = (SimpleType, SimpleType)

showTypeEquation :: TypeEquation -> String
showTypeEquation (t1, t2) = show t1 ++ " = " ++ show t2

showTypeEquations :: [TypeEquation] -> String
showTypeEquations eqs = unlines $ map showTypeEquation eqs

type Substitution = [(TypeID, SimpleType)]

showSubstitution :: Substitution -> String
showSubstitution = showTypeEquations . map (\(i, t) -> (TVar i, t))


data InferResult =
    SuccessfullyTyped SimpleType
    | Untypable
    | ConstraintInsufficient SimpleType
    deriving (Show, Eq)

infer :: Expr -> InferResult
infer e =
    case infer' e of
        Nothing -> Untypable
        Just t ->
            if null $ ftv t then SuccessfullyTyped t else ConstraintInsufficient t


infer' :: Expr -> Maybe SimpleType
infer' e = do
    (eqs, t) <- extract e
    substitution <- unify eqs
    return $ substitute substitution t

extract :: Expr -> Maybe ([TypeEquation], SimpleType)
extract e = do
    (_, eqs, t) <- extract' newTypeIDGenerator Map.empty e
    return (eqs, t)

extract' :: TypeIDGenerator -> TypeEnv -> Expr -> Maybe (TypeIDGenerator , [TypeEquation], SimpleType)
extract' gen tenv (EVar v) = do
    t <- Map.lookup v tenv
    return (gen, [], t)
extract' gen tenv (EBool _) = return (gen, [], TBool)
extract' gen tenv (EInt _) = return (gen, [], TInt)
extract' gen tenv (EAdd e1 e2) = extractIntOp gen tenv e1 e2
extract' gen tenv (ESub e1 e2) = extractIntOp gen tenv e1 e2
extract' gen tenv (EMul e1 e2) = extractIntOp gen tenv e1 e2
extract' gen tenv (EDiv e1 e2) = extractIntOp gen tenv e1 e2
extract' gen tenv (EAnd e1 e2) = extractBoolOp gen tenv e1 e2
extract' gen tenv (EOr e1 e2) = extractBoolOp gen tenv e1 e2
extract' gen tenv (ENot e) = do
    (gen1, eqs, t) <- extract' gen tenv e
    return (gen1, (t, TBool) : eqs, TBool)
extract' gen tenv (ELT e1 e2) = extractCompOp gen tenv e1 e2
extract' gen tenv (EEqInt e1 e2) = extractCompOp gen tenv e1 e2
extract' gen tenv (EIf e1 e2 e3) = do
    (gen1, eqs1, t1) <- extract' gen tenv e1
    (gen2, eqs2, t2) <- extract' gen1 tenv e2
    (gen3, eqs3, t3) <- extract' gen2 tenv e3
    let eqs = (t1, TBool) : (t2, t3) : eqs1 ++ eqs2 ++ eqs3
    return (gen3, eqs, t2)
extract' gen tenv (ELet x e1 e2) = do
    (gen1, eqs1, t1) <- extract' gen tenv e1
    let newTEnv = Map.insert x t1 tenv
    (gen2, eqs2, t2) <- extract' gen1 newTEnv e2
    return (gen2, eqs1 ++ eqs2, t2)
extract' gen tenv (EAbs x e) = do
    let (tv, gen') = genFreshTVar gen
    let newTEnv = Map.insert x tv tenv
    (gen1, eqs1, t1) <- extract' gen' newTEnv e
    return (gen1, eqs1, TFun tv t1)
extract' gen tenv (EApp e1 e2) = do
    (gen1, eqs1, t1) <- extract' gen tenv e1
    (gen2, eqs2, t2) <- extract' gen1 tenv e2
    let (tv, gen') = genFreshTVar gen2
    let eqs = (t1, TFun t2 tv) : eqs1 ++ eqs2
    return (gen', eqs, tv)
extract' gen tenv ENil = $notImplemented -- リストはまだサポートしない
extract' gen tenv (ECons e1 e2) = $notImplemented -- リストはまだサポートしない
extract' gen tenv (EMatch e1 e2 x1 x2 e3) = $notImplemented -- リストはまだサポートしない


extractIntOp :: TypeIDGenerator -> TypeEnv -> Expr -> Expr -> Maybe (TypeIDGenerator, [TypeEquation], SimpleType)
extractIntOp gen tenv e1 e2 = do
    (gen1, eqs1, t1) <- extract' gen tenv e1
    (gen2, eqs2, t2) <- extract' gen1 tenv e2
    return (gen2, (t1, TInt) : (t2, TInt) : eqs1 ++ eqs2, TInt)

extractBoolOp :: TypeIDGenerator -> TypeEnv -> Expr -> Expr -> Maybe (TypeIDGenerator, [TypeEquation], SimpleType)
extractBoolOp gen tenv e1 e2 = do
    (gen1, eqs1, t1) <- extract' gen tenv e1
    (gen2, eqs2, t2) <- extract' gen1 tenv e2
    return (gen2, (t1, TBool) : (t2, TBool) : eqs1 ++ eqs2, TBool)

extractCompOp :: TypeIDGenerator -> TypeEnv -> Expr -> Expr -> Maybe (TypeIDGenerator, [TypeEquation], SimpleType)
extractCompOp gen tenv e1 e2 = do
    (gen1, eqs1, t1) <- extract' gen tenv e1
    (gen2, eqs2, t2) <- extract' gen1 tenv e2
    return (gen2, (t1, TInt) : (t2, TInt) : eqs1 ++ eqs2, TBool)

unify :: [TypeEquation] -> Maybe Substitution
unify [] = return []
unify ((t1, t2) : eqs)
    | t1 == t2 = unify eqs
unify ((t, TVar i) : eqs) =
        if i `elem` ftv t
        then Nothing
        else do
            let substitutedEqs = substituteEqs [(i, t)] eqs
            substitution <- unify substitutedEqs
            return $ (i, substitute substitution t) :substitution
unify ((TVar i, t ) : eqs) = unify ((t, TVar i) : eqs)
unify ((TFun t11 t12, TFun t21 t22) : eqs) = unify ((t11, t21) : (t12, t22) : eqs)
unify _ = Nothing

substituteEqs :: Substitution -> [TypeEquation] -> [TypeEquation]

substituteEqs substitution = map (\(l, r) -> (substitute substitution l, substitute substitution r))

substitute :: Substitution -> SimpleType ->SimpleType
substitute substitution t = foldl (flip substituteOne) t substitution

substituteOne :: (TypeID, SimpleType) -> SimpleType -> SimpleType
substituteOne (i, t) (TVar j)
    | i == j    = t
    | otherwise = TVar j
substituteOne (i, t) TBool = TBool
substituteOne (i, t) TInt = TInt
substituteOne (i, t) (TFun t1 t2) = TFun (substituteOne (i, t) t1) (substituteOne (i, t) t2)

ftv :: SimpleType -> [TypeID]
ftv (TVar i) = [i]
ftv TBool = []
ftv TInt = []
ftv (TFun t1 t2) = ftv t1 ++ ftv t2
