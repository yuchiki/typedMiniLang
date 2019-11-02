{-# LANGUAGE TemplateHaskell #-}

module SimpleTyping where

import Development.Placeholders
import Typ
import Expr


typeSimply :: TypEnv -> Expr -> Typ
typeSimply tenv (EVar variable) = $notImplemented
typeSimply tenv (EBool _) = $notImplemented
typeSimply tenv (EInt _) = $notImplemented
typeSimply tenv (EAdd e1 e2) = $notImplemented
typeSimply tenv (ESub e1 e2) = $notImplemented
typeSimply tenv (EMul e1 e2) = $notImplemented
typeSimply tenv (EDiv e1 e2) = $notImplemented
typeSimply tenv (EAnd e1 e2) = $notImplemented
typeSimply tenv (EOr e1 e2) = $notImplemented
typeSimply tenv (ENot e) = $notImplemented
typeSimply tenv (ELT e1 e2) = $notImplemented
typeSimply tenv (EEqInt e1 e2) = $notImplemented
typeSimply tenv (EIf e1 e2 e3) = $notImplemented
typeSimply tenv (ELet x e1 e2) = $notImplemented
typeSimply tenv (EAbs x e) = $notImplemented
typeSimply tenv (EApp e1 e2) = $notImplemented
typeSimply tenv ENil = $notImplemented
typeSimply tenv (ECons e1 e2) = $notImplemented
typeSimply tenv (EMatch e1 e2 x1 x2 e3) = $notImplemented
