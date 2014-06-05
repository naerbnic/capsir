module Capsir where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Cont = Cont
    ![String]  -- Formal parameter names
    !CpsExpr   -- The body expression of the continuation

data Const = ConstFunc   -- A const creating function
               !String   -- The name of the function
               ![Const]  -- The arguments to the function
           | ConstInt !Int -- A constant int
           | ConstString !String  -- A constant string
           | ConstFloat !Double  -- A constant float

data Value = ContValue !Cont  -- A literal continuation value
           | VarValue !String  -- A variable reference value
           | ConstValue !Const  -- A constant data value

data CpsExpr = Apply ![Value]  -- The arguments to apply to the continuation
                     !Value    -- A value that evaluates to a continuation
             | Inst !String   -- The instruction name
                    ![Value]  -- The instruction parameters
                    ![Value]  -- | A list continuation values that the inst can 
                              -- call.
             | Fix ![(String, Cont)]  -- A mapping from function names to conts
                   !CpsExpr           -- The expression to execute in this environment
             | Exit  -- Exits the program

-- Runtime semantic information

data ContInst = ContInst Env Cont
data RuntimeValue = InstRuntimeValue ContInst
                  | ConstRuntimeValue Const

data Env = EmptyEnv
         | FrameEnv (Map String RuntimeValue) Env


lookupEnv :: String -> Env ->  Maybe RuntimeValue
lookupEnv _ EmptyEnv = Nothing
lookupEnv name (FrameEnv envMap child) =
    case Map.lookup name envMap of
      Just val -> Just val
      Nothing -> lookupEnv name child


-- Evaluates a syntactic Value to a RuntimeValue in the given environment
eval :: Value -> Env -> RuntimeValue
eval (ContValue cont) env = InstRuntimeValue (ContInst env cont)
eval (VarValue name) env = fromJust (lookupEnv name env)
eval (ConstValue const) _ = ConstRuntimeValue const

-- | Evalues a syntactic value as eval, but forces it to be a Continuation
-- Instance
evalAsCont :: Value -> Env -> ContInst
evalAsCont val env = case eval val env of
    InstRuntimeValue inst -> inst
    _ -> error "Expected a continuation; Got something else"

zipOrError :: [a] -> [b] -> [(a, b)]
zipOrError (a:ax) (b:bx) = (a, b) : zipOrError ax bx
zipOrError [] [] = []
zipOrError _ _ = error "Mismatched input length in zip"

data UserInst = UserInst {
  
}

data ExecState = ExecState Env CpsExpr

applySecond :: (a -> b) -> (c, a) -> (c, b)
applySecond f (c, a) = (c, f a)

step :: ExecState -> Maybe ExecState
step (ExecState _ Exit) = Nothing

step (ExecState env (Apply args val)) =
    let ContInst contEnv (Cont params nextExpr) = evalAsCont val env
        runtimeArgs = map (`eval` env) args
        newFrame = Map.fromList $ zipOrError params runtimeArgs
        newEnv = FrameEnv newFrame contEnv
    in Just $ ExecState newEnv nextExpr

step (ExecState env (Fix bindings nextExpr)) =
    -- Need to use a cyclic data structure to represent the environment
    let newEnv = FrameEnv contMap env

        createContInst :: Cont -> RuntimeValue
        createContInst c = InstRuntimeValue $ ContInst newEnv c

        runtimeValPairs = map (applySecond createContInst) bindings

        contMap = Map.fromList runtimeValPairs
    in Just $ ExecState newEnv nextExpr

-- step (ExecState env (Inst
