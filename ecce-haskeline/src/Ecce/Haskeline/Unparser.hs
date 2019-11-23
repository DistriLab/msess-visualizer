module Ecce.Haskeline.Unparser where

import Ecce.Haskeline.Interpreter
import Ecce.Unparser

main :: IO ()
main = mainHaskeline commandOutputs incommandOutput
