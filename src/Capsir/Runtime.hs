module Capsir.Runtime where

import Capsir
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- | An instance of a continuation. Binds an environment to the
-- continuation itself.
data ContInst = ContInst Env Cont

-- | A runtime value. Each variable is bound to one during continuation
-- execution
data RuntimeValue
    -- | A continuation instance value
    = InstRuntimeValue ContInst
    -- | A constant value
    | ConstRuntimeValue Const

-- | A variable binding environment.
data Env
    -- | The empty binding environment.
    = EmptyEnv
    -- | A single environment frame. Contains the mappings from variables to
    -- runtime values for this frame, and references the next environment in
    -- the chain.
    | FrameEnv (Map String RuntimeValue) Env

-- | Looks up a variable in the environment.
lookupEnv
    :: String               -- ^ The variable to look up
    -> Env                  -- ^ The environment to look it up in
    ->  Maybe RuntimeValue  -- ^ Nothing if that value is not bound,
                            -- otherwise @Just value@ where value is the bound
                            -- value.
lookupEnv _ EmptyEnv = Nothing
lookupEnv name (FrameEnv envMap child) =
    case Map.lookup name envMap of
      Just val -> Just val
      Nothing -> lookupEnv name child


-- | Evaluates a syntactic Value to a RuntimeValue in the given environment
eval :: Value -> Env -> RuntimeValue
eval (ContValue cont) env = InstRuntimeValue (ContInst env cont)
eval (VarValue name) env = fromJust (lookupEnv name env)
eval (ConstValue constVal) _ = ConstRuntimeValue constVal

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

-- | The type of a user-defined instruction function. Takes a list of runtime
-- values, then generates a new set of runtime value outputs along with an
-- index into the continuation list which will be executed next.
type InstFunc = [RuntimeValue] -> (Int, [RuntimeValue])

data InstructionSet = UserInst
    { instructions :: Map String InstFunc
    }

data ExecState = ExecState Env CpsExpr

-- | Applies a set of actual parameters to a continuation to
-- generate a new execution state.
applyCont :: RuntimeValue -> [RuntimeValue] -> ExecState
applyCont (InstRuntimeValue inst) args =
    let ContInst contEnv (Cont params nextExpr) = inst
        newFrame = Map.fromList $ zipOrError params args
        newEnv = FrameEnv newFrame contEnv
    in ExecState newEnv nextExpr
applyCont _ _ = error "Expected a continuation instance"

-- | Applies a function to the second item in a pair.
applySecond :: (a -> b) -> (c, a) -> (c, b)
applySecond f (c, a) = (c, f a)

-- | Takes a single step through the given execution state. Returns either a
-- new execution state, or a single runtime value if the program exited.
step :: InstructionSet -> ExecState -> Either ExecState RuntimeValue
step _ (ExecState env (Exit val)) = Right $ eval val env

step _ (ExecState env (Apply args val)) =
    let runtimeArgs = map (`eval` env) args
    in Left $ applyCont (eval val env) runtimeArgs

step _ (ExecState env (Fix bindings nextExpr)) =
    -- Need to use a cyclic data structure to represent the environment
    let newEnv = FrameEnv contMap env

        createContInst :: Cont -> RuntimeValue
        createContInst c = InstRuntimeValue $ ContInst newEnv c

        runtimeValPairs = map (applySecond createContInst) bindings

        contMap = Map.fromList runtimeValPairs
    in Left $ ExecState newEnv nextExpr

step instSet (ExecState env (Inst instName args conts)) = 
    let instFunc = instructions instSet Map.! instName
        runtimeArgs = map (`eval` env) args
        (branchIndex, results) = instFunc runtimeArgs
        nextCont = conts !! branchIndex
    in Left $ applyCont (eval nextCont env) results

-- | Calls the function on the init, and loops while the output is left, 
-- feeding the value back into the function. When it returns Right, yields the
-- value.
eitherLoop :: (a -> Either a b) -> a -> b
eitherLoop stepFunc initVal =
    let go (Left a) = go (stepFunc a)
        go (Right b) = b
    in go (stepFunc initVal)

-- | A function that given a user-defined instruction set and an initial
-- expression, evaluates it until completion.
runCont :: InstructionSet -> CpsExpr -> RuntimeValue
runCont instSet expr =
    eitherLoop (step instSet) (ExecState EmptyEnv expr)
