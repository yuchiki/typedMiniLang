module Typ where

import qualified Data.Map as Map
import Variable

data Typ =
    TBool
    | TInt
    | TFun Typ Typ
    | TList Typ

type TypEnv = Map.Map Variable Typ
