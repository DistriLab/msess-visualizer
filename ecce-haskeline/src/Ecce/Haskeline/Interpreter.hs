{-
 - SECTION MODULE
 -}
module Ecce.Haskeline.Interpreter where

{-
 - SECTION IMPORTS
 -}
import Control.Monad.IO.Class (liftIO)
import Ecce.Base (SParsec, extractParse)
import Ecce.Interpreter (interpret)
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )

-- Every output function must have the same inputs
-- So that the interpret function can be generalized
type Output = (String, [String], String) -> IO ()

mainHaskeline :: [(String, Output)] -> Output -> IO ()
mainHaskeline commandOutputs incommandOutput = do
  welcome
  runInputT defaultSettings $
    interpreterHaskeline commandOutputs incommandOutput

welcome :: IO ()
welcome = do
  mapM_ putStrLn $ "Welcome!" : "Type \"help\" for more information." : "" : []

interpreterHaskeline :: [(String, Output)] -> Output -> InputT IO ()
interpreterHaskeline commandOutputs incommandOutput = do
  mInputLine <- getInputLine "ecce> "
  maybe
    (outputStrLn "Quitting")
    (\inputLine ->
       (liftIO $ interpret commandOutputs incommandOutput inputLine) >>
       interpreterHaskeline commandOutputs incommandOutput)
    mInputLine
