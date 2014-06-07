import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import System.Exit (exitFailure)

import qualified TestRuntime

import qualified Distribution.TestSuite as Cabal

test1 = assertEqual "" 1 2
test2 = assertEqual "" 3 4

main = defaultMain
    [ testCase "Simple Program Executes" TestRuntime.test
    ]
          
