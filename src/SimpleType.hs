module SimpleType where

import Variable
import qualified Data.Map as Map

type TypeID = Int

data SimpleType =
    TVar TypeID
    | TBool
    | TInt
    | TFun SimpleType SimpleType
    deriving(Eq)

instance Show SimpleType where
    show (TVar i)
        | i <= 23   = greeks !! i
        | otherwise = "ty" ++ show i
        where
            greeks = ["α", "β", "γ", "δ", "ε", "ζ", "χ", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]
    show TBool = "bool"
    show TInt = "int"
    show (TFun t1 t2) = parensLeft t1 ++ " -> " ++ show t2

parensLeft :: SimpleType -> String
parensLeft (TFun t1 t2) = "(" ++ show (TFun t1 t2) ++ ")"
parensLeft t = show t

type TypeEnv = Map.Map Variable SimpleType

newtype TypeIDGenerator =  TypeIDGenerator TypeID

newTypeIDGenerator :: TypeIDGenerator
newTypeIDGenerator = TypeIDGenerator 0

genFreshTVar :: TypeIDGenerator -> (SimpleType, TypeIDGenerator)
genFreshTVar (TypeIDGenerator i) = (TVar i, TypeIDGenerator (i + 1))
