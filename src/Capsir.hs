module Capsir
    ( Cont(..)
    , Const(..)
    , Value(..)
    , CpsExpr(..)
    ) where


-- | A continuation with a formal parameter list and body expression.
data Cont = Cont ![String] !CpsExpr
  deriving Show

-- | A value literal.
data Const
    -- | A user-defined literal function with a name and constant param list.
    = ConstFunc !String ![Const]
    -- | A constant integer
    | ConstInt !Int
    -- | A constant string
    | ConstString !String
    -- | A constant float
    | ConstFloat !Double
  deriving Show

data Value = ContValue !Cont    -- ^ A literal continuation value
           | VarValue !String   -- ^ A variable reference value
           | ConstValue !Const  -- ^ A constant data value
  deriving Show

data CpsExpr 
    -- | Applies the actual parameter values to the continuation value.
    = Apply ![Value] !Value
    -- | A user-defined instruction that takes the actual parameters, and
    -- selects one of the continuations in the second list by index to
    -- execute with new values.
    | Inst !String ![Value] ![Value]
    -- | A fixed-point instruction that ensures that all continuations defined
    -- in the bindings have all of the bindings in the fix in scope.
    | Fix ![(String, Cont)] !CpsExpr
    -- | Exits the program with the given value as a result.
    | Exit !Value
  deriving Show

