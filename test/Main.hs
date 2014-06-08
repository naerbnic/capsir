import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import System.Exit (exitFailure)

import qualified TestRuntime
import qualified TestParse

import qualified Distribution.TestSuite as Cabal

main = defaultMain $
    [ testCase "Simple Program Executes" TestRuntime.tests ] ++
    hUnitTestToTests TestParse.tests
          
