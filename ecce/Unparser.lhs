\subsection{Unparser}

The unparser, or pretty-printer, is just the inverse function of the parser.
That is, an unparser converts an ADT into a string.  Implementing this with
\textit{partial-isomorphism}s is not only straightforward, but also has the
advantage of being isomorphic.  That is, an unparsed ADT can be parsed to get
the same ADT, and a parsed string can be unparsed to get the same string.

%if False
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
\end{code}
%endif

Unparsing is done by simply passing the partial isomorphism
\textit{parseGlobalProtocol} and a \textit{GlobalProtocol} to
\textit{invertible-syntax}'s \textit{print} function.  We then unwrap the
\textit{Maybe} result, and display an error if \textit{Nothing} is returned.
\par
\textit{un} takes a \textit{GlobalProtocol} as input instead of any well-formed
expression in typical pretty-printers.  This is because partial isomorphisms
and existential types do not mix well.  It is possible to wrap a existential
type \textit{AnyExpr} around all expression types (e.g. \textit{Pure},
\textit{GlobalProtocol}, etc...).  However, deconstructing \textit{AnyExpr} is
impossible, since we do not know which expression type is actually being
wrapped inside \textit{AnyExpr}.  Therefore, we can only construct existential
types, but not destruct existential types.  So, we are unable to establish an
isomorphism between the existential and the expression types.

\begin{code}
un :: GlobalProtocol -> String
un e =
  case Text.Syntax.Printer.Naive.print parseGlobalProtocol e of
    Just x -> x
    Nothing -> error "Print error: " ++ show e
\end{code}
