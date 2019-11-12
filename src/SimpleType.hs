module SimpleType where

import Variable
import qualified Data.Map as Map

type TypeID = Int

data SimpleType =
    TVar TypeID
    | TBool
    | TInt
    | TFun SimpleType SimpleType
    deriving(Show, Eq)

type TypeEnv = Map.Map Variable SimpleType

newtype TypeIDGenerator =  TypeIDGenerator TypeID

newTypeIDGenerator :: TypeIDGenerator
newTypeIDGenerator = TypeIDGenerator 0

genFreshTVar :: TypeIDGenerator -> (SimpleType, TypeIDGenerator)
genFreshTVar (TypeIDGenerator i) = (TVar i, TypeIDGenerator (i + 1))
