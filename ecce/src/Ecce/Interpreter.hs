{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE StandaloneDeriving #-} -- Allows `deriving` on its own line
{-# LANGUAGE GADTs #-} -- Allows constrained ASTs
{-# LANGUAGE UndecidableInstances #-} -- Allows >1 StandaloneDeriving
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns

{-
 - SECTION MODULE
 -}
module Ecce.Interpreter where

{-
 - SECTION IMPORTS
 -}
import Control.Monad.IO.Class (liftIO)
import Ecce.Base (SParsec, extractParse)
import Text.Parsec ((<|>), anyChar, many, space, string, try)

-- Every output function must have the same inputs
-- So that the interpret function can be generalized
type Output = (String, [String], String) -> IO ()

mainRegular :: [(String, Output)] -> Output -> IO ()
mainRegular commandOutputs incommandOutput = do
  welcome
  interpreterRegular commandOutputs incommandOutput

welcome :: IO ()
welcome = do
  mapM_ putStrLn $ "Welcome!" : "Type \"help\" for more information." : "" : []

interpreterRegular :: [(String, Output)] -> Output -> IO ()
interpreterRegular commandOutputs incommandOutput = do
  putStr "ecce> "
  inputLine <- getLine
  interpret commandOutputs incommandOutput inputLine
  interpreterRegular commandOutputs incommandOutput

-- Parses inputLine for command, parsers extracted from keys of input (1)
-- Looks up parsed command in input (1), to determine correct function to call
-- Inputs:
-- (1) map of commands to output functions
-- (2) output function when map lookup fails
-- (3) inputLine to be interpreted
-- Output:
-- (1) IO action
interpret :: [(String, Output)] -> Output -> String -> IO ()
interpret commandOutputs incommandOutput inputLine =
  maybe
    (incommandOutput (inputLine, commands, restInputLine))
    ($ (inputLine, commands, restInputLine))
    (lookup command commandOutputs)
    -- TODO fix double parsing
    -- TODO if "error" is a legitimate command, then if extractParse fails, the 
    -- interpreter will not detect the failure
  where
    commands = (fst . unzip) commandOutputs
    command =
      either (const "error") id $ extractParse (parseCommand commands) inputLine
    restInputLine =
      either (const "error") id $
      extractParse (parseRestInputLine commands) inputLine

parseCommand :: [String] -> SParsec String
parseCommand commands = foldl (\p p' -> p <|> try p') (try h) t
  where
    (h:t) = map string commands

parseRestInputLine :: [String] -> SParsec String
parseRestInputLine commands =
  parseCommand commands >> many space >> many anyChar
