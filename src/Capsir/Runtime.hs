module Capsir.Runtime where

import Capsir
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- | An instance of a continuation. Binds an environment to the
-- continuation itself.
data ContInst v = ContInst (Env v) Cont

-- | A runtime value. Each variable is bound to one during continuation
-- execution
data RuntimeValue v
    -- | A continuation instance value
    = InstRuntimeValue (ContInst v)
    -- | A constant value
    | ConstRuntimeValue v

-- | A variable binding environment.
data Env v
    -- | The empty binding environment.
    = EmptyEnv
    -- | A single environment frame. Contains the mappings from variables to
    -- runtime values for this frame, and references the next environment in
    -- the chain.
    | FrameEnv (Map String (RuntimeValue v)) (Env v)

-- | The type of a user-defined instruction function. Takes a list of runtime
-- values, then generates a new set of runtime value outputs along with an
-- index into the continuation list which will be executed next.
type InstFunc m v = [RuntimeValue v] -> m (Int, [RuntimeValue v])

data LitParamVal v = LitParamValConst v
                   | LitParamValInt !Int
                   | LitParamValString !String
                   | LitParamValFloat !Double

-- | Type type of a user-defined literal function. Takes a list of literal
-- parameters, and returns a value of type a, which is the user-defined runtime
-- value type.
type LitFunc v = [LitParamVal v] -> v

data InstructionSet m v = UserInst
    { instructions :: Map String (InstFunc m v)
    , literals :: Map String (LitFunc v)
    }

-- | Looks up a variable in the environment.
lookupEnv
    :: String                  -- ^ The variable to look up
    -> Env v                   -- ^ The environment to look it up in
    -> Maybe (RuntimeValue v)  -- ^ Nothing if that value is not bound,
                               -- otherwise @Just value@ where value is the bound
                               -- value.
lookupEnv _ EmptyEnv = Nothing
lookupEnv name (FrameEnv envMap child) =
    case Map.lookup name envMap of
      Just val -> Just val
      Nothing -> lookupEnv name child

evalLit :: InstructionSet m v -> Literal -> v
evalLit instSet (Literal name params) =
    let litFunc = literals instSet Map.! name

        evalParam (LitParamLit literal) =
            LitParamValConst $ evalLit instSet literal
        evalParam (LitParamInt i) = LitParamValInt i
        evalParam (LitParamString s) = LitParamValString s
        evalParam (LitParamFloat f) = LitParamValFloat f

        evaluatedParams = map evalParam params
    in litFunc evaluatedParams

-- | Evaluates a syntactic Value to a RuntimeValue in the given environment
eval :: InstructionSet m v -> Value -> Env v -> RuntimeValue v
eval _ (ContValue cont) env = InstRuntimeValue (ContInst env cont)
eval _ (VarValue name) env = fromJust (lookupEnv name env)
eval instSet (LitValue lit) _ = ConstRuntimeValue $ evalLit instSet lit

-- | Evalues a syntactic value as eval, but forces it to be a Continuation
-- Instance
evalAsCont :: InstructionSet m v -> Value -> Env v -> ContInst v
evalAsCont instSet val env = case eval instSet val env of
    InstRuntimeValue inst -> inst
    _ -> error "Expected a continuation; Got something else"

zipOrError :: [a] -> [b] -> [(a, b)]
zipOrError (a:ax) (b:bx) = (a, b) : zipOrError ax bx
zipOrError [] [] = []
zipOrError _ _ = error "Mismatched input length in zip"



data ExecState v = ExecState (Env v) CpsExpr

-- | Applies a set of actual parameters to a continuation to
-- generate a new execution state.
applyCont :: RuntimeValue v -> [RuntimeValue v] -> ExecState v
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
step :: Monad m => InstructionSet m v -> ExecState v -> m (Either (ExecState v) (RuntimeValue v))
step instSet (ExecState env (Exit val)) = return $ Right $ eval instSet val env

step instSet (ExecState env (Apply args val)) =
    let runtimeArgs = map (\arg -> eval instSet arg env) args
    in return $ Left $ applyCont (eval instSet val env) runtimeArgs

step _ (ExecState env (Fix bindings nextExpr)) =
    -- Need to use a cyclic data structure to represent the environment
    let newEnv = FrameEnv contMap env

        createContInst c = InstRuntimeValue $ ContInst newEnv c

        runtimeValPairs = map (applySecond createContInst) bindings

        contMap = Map.fromList runtimeValPairs
    in return $ Left $ ExecState newEnv nextExpr

step instSet (ExecState env (Inst instName args conts)) = 
    let instFunc = instructions instSet Map.! instName
        runtimeArgs = map (\arg -> eval instSet arg env) args
    in do 
        (branchIndex, results) <- instFunc runtimeArgs
        let nextCont = conts !! branchIndex
        return $ Left $ applyCont (eval instSet nextCont env) results

-- | Calls the function on the init, and loops while the output is left, 
-- feeding the value back into the function. When it returns Right, yields the
-- value.
eitherLoop :: Monad m => (a -> m (Either a b)) -> a -> m b
eitherLoop stepFunc initVal =
    let go (Left a) = stepFunc a >>= go
        go (Right b) = return b
    in stepFunc initVal >>= go

-- | A function that given a user-defined instruction set and an initial
-- expression, evaluates it until completion.
runCont :: Monad m => InstructionSet m v -> CpsExpr -> m (RuntimeValue v)
runCont instSet expr =
    eitherLoop (step instSet) (ExecState EmptyEnv expr)
