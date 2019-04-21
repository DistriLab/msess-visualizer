\begin{code}
{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows patterm match on GADT

{-
 - SECTION MODULE
 -}
module Unparser where

{-
 - SECTION IMPORTS
 -}
import Base (extractParse)
import Interpreter (Output, mainHaskeline)
import Parser (GlobalProtocol, parseGlobalProtocol)
import qualified Text.Syntax.Printer.Naive (print)

{-
 - SECTION USER INTERFACE
 -}
type Test = (Integer, String, String)

main :: IO ()
main = mainHaskeline commandOutputs incommandOutput

commandOutputs :: [(String, Output)]
commandOutputs =
  [ ( "help"
    , \(_, commands, _) ->
        mapM_ putStrLn $ "Here are a list of commands:" : commands)
  ]

incommandOutput :: Output
incommandOutput =
  \(inputLine, _, _) ->
    putStrLn $ un $ head $ extractParse parseGlobalProtocol inputLine

{-
 - SUBSECTION UNPARSER
 -}
un :: GlobalProtocol -> String
un e =
  case Text.Syntax.Printer.Naive.print parseGlobalProtocol e of
    Just x -> x
    Nothing -> error "Print error: " ++ show e
\end{code}
