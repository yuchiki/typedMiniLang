module Type where

import qualified Data.Map as Map
import Variable

data Type =
    TBool
    | TInt
    | TFun Type Type
    | TList Type

type TypeEnv = Map.Map Variable Type
