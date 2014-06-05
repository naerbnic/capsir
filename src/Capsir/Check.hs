module Capsir.Check where

-- import Capsir
-- import Data.Map (Map)
-- import qualified Data.Map as Map

data Type = ContType [Type]
          | LitType 
