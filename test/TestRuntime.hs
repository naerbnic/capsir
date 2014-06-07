module TestRuntime where

import Control.Monad.Identity
import qualified Data.Map as Map
import Capsir
import Capsir.Runtime
import Test.HUnit

-- | A very simple instruction set that has Int as its value type, and can add
-- two numbers together, and check if one integer is less than another.
intInstSet :: InstructionSet Identity Int
intInstSet = InstructionSet
    { instructions = Map.fromList [
          ("add", \[x, y] -> Identity $ (0, [x + y])),
          ("lt", \[x, y] -> Identity $ if x < y then (0, []) else (1, []))
          ],
      literals = Map.fromList [
          ("int", \[LitParamValInt i] -> i)
          ]
    }

intLit :: Int -> Value
intLit i = LitValue $ Literal "int" [LitParamInt i]

simpleProgram :: CpsExpr
simpleProgram = 
  Fix [
    ("f", Cont ["x"] 
        (Inst "lt" [(VarValue "x"), intLit 5] [
            ContValue $ Cont [] (Inst "add" [(VarValue "x"), intLit 1] [(VarValue "f")]),
            ContValue $ Cont [] (Exit (VarValue "x"))
        ]))
  ] (Apply [intLit 0] (VarValue "f"))

test = assertEqual "The program evaluates to 5"
    (let ConstRuntimeValue i = runIdentity (runCont intInstSet simpleProgram)
     in i)
    5
  
