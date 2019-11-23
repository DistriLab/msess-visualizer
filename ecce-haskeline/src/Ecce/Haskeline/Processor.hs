module Ecce.Haskeline.Processor where

import Ecce.Haskeline.Interpreter
import Ecce.Processor

main :: IO ()
main = mainHaskeline commandOutputs incommandOutput
