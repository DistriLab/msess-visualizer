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
module Interpreter where

import Base (SParsec, extractParse)

{-
 - SECTION IMPORTS
 -}
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )
import Text.Parsec
  ( Parsec
  , (<|>)
  , anyChar
  , many
  , optionMaybe
  , space
  , string
  , try
  )

-- Every output function must have the same inputs
-- So that the interpret function can be generalized
type Output = (String, [String], String) -> IO ()

mainHaskeline :: [(String, Output)] -> Output -> IO ()
mainHaskeline commandOutputs incommandOutput = do
  welcome
  runInputT defaultSettings $ interpreter commandOutputs incommandOutput

welcome :: IO ()
welcome = do
  mapM_ putStrLn $ "Welcome!" : "Type \"help\" for more information." : "" : []

interpreter :: [(String, Output)] -> Output -> InputT IO ()
interpreter commandOutputs incommandOutput = do
  mInputLine <- getInputLine "ecce> "
  case mInputLine of
    Nothing -> outputStrLn "Quitting"
    Just inputLine ->
      (liftIO $ interpret commandOutputs incommandOutput inputLine) >>
      interpreter commandOutputs incommandOutput

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
    -- TODO find better way to extract parsed expression than (Right .. =)
  where
    commands = (fst . unzip) commandOutputs
    Right (Just command) = extractParse (parseCommand commands) inputLine
    Right restInputLine = extractParse (parseRestInputLine commands) inputLine

parseCommand :: [String] -> SParsec (Maybe String)
parseCommand commands = optionMaybe $ foldl (\p p' -> p <|> try p') (try h) t
  where
    (h:t) = map string commands

parseRestInputLine :: [String] -> SParsec String
parseRestInputLine commands =
  parseCommand commands >> many space >> many anyChar
