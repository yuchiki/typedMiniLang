{-# LANGUAGE TemplateHaskell #-}

module SimpleTyping where

import qualified Data.Map as Map
import Development.Placeholders
import SimpleType
import Expr

type TypeEquation = (SimpleType, SimpleType)

extract :: [TypeEnv] -> Expr -> ([TypeEquation], SimpleType)
extract tenv (EVar v) = $notImplemented
extract tenv (EBool _) = $notImplemented
extract tenv (EInt _) = $notImplemented
extract tenv (EAdd e1 e2) = $notImplemented
extract tenv (ESub e1 e2) = $notImplemented
extract tenv (EMul e1 e2) = $notImplemented
extract tenv (EDiv e1 e2) = $notImplemented
extract tenv (EAnd e1 e2) = $notImplemented
extract tenv (EOr e1 e2) = $notImplemented
extract tenv (ENot e) = $notImplemented
extract tenv (ELT e1 e2) = $notImplemented
extract tenv (EEqInt e1 e2) = $notImplemented
extract tenv (EIf e1 e2 e3) = $notImplemented
extract tenv (ELet x e1 e2) = $notImplemented
extract tenv (EAbs x e) = $notImplemented
extract tenv (EApp e1 e2) = $notImplemented
extract tenv ENil = $notImplemented
extract tenv (ECons e1 e2) = $notImplemented
extract tenv (EMatch e1 e2 x1 x2 e3) = $notImplemented
