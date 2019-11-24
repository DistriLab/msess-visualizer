module Ecce.Haskeline.Parser where

import Ecce.Haskeline.Interpreter
import Ecce.Parser

main :: IO ()
main = mainHaskeline commandOutputs incommandOutput
