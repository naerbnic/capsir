module Capsir.Check where

import Capsir
-- import Data.Map (Map)
-- import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data BaseType t = ContType [t]
                | ConstType 
  deriving (Eq, Show)

freeVarsFromList :: (a -> Set String) -> [a] -> Set String
freeVarsFromList f = Set.unions . map f


freeContVars :: Cont -> Set String
freeContVars (Cont params expr) =
  Set.difference (freeExprVars expr) (Set.fromList params)


freeValueVars :: Value -> Set String
freeValueVars (ContValue cont) = freeContVars cont
freeValueVars (VarValue name) = Set.singleton name
freeValueVars (LitValue _) = Set.empty


freeValueVarsFromList :: [Value] -> Set String
freeValueVarsFromList = freeVarsFromList freeValueVars


freeExprVars :: CpsExpr -> Set String

freeExprVars (Apply args cont) =
    let contVars = freeValueVars cont
        argVars = Set.unions $ map freeValueVars args
    in Set.union contVars argVars

freeExprVars (Inst _ args conts) =
    let contVars = freeValueVarsFromList conts
        argsVars = freeValueVarsFromList args
    in Set.union contVars argsVars

freeExprVars (Fix pairs expr) =
    let (names, conts) = unzip pairs
        contVars = freeVarsFromList freeContVars conts
        exprVars = freeExprVars expr
    in Set.difference (Set.union contVars exprVars) (Set.fromList names)

freeExprVars (Exit val) = freeValueVars val

data Type = VarT Int
          | ContUnknown
          | BaseT (BaseType Type)
  deriving (Eq, Show)

data Fact = Assert Int Type
          | Unify Int String
  deriving (Eq, Show)

data TypedExpr
    = TApply ![Value] !Value


