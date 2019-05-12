\defslide{UnparserIntroduction}{
The unparser is just the inverse function of the parser.
}

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

\defslide{UnparserUn}{
Unparsing is done by simply passing the partial isomorphism
\textit{parseGlobalProtocol} and a \textit{GlobalProtocol} to
\textit{invertible-syntax}'s \textit{print} function.  We then unwrap the
\textit{Maybe} result, and display an error if \textit{Nothing} is returned.

%if False
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
\par
Note that by using partial isomorphisms, we guarantee a one-to-one
correspondence betwene the parsed string and the printed string.  This avoids
any confusion to the user, that stems from a mismatch between the printed
string and the parsed string.
\par
There is also a benefit of using partial isomorphisms: only the parser needs to
be defined, and we get an unparser for free.  This reduces the amount of
maintenance work, since for parsers defined without partial isomorphisms, the
unparser also has to be changed whenever there are changes to the parser.  This
contrasts with a parser defined with partial isomorphisms: the unparser is
dependent on the parser, so only the parser needs to be changed.
\par
It may seem logical to extend partial isomorphisms to all transformations
within \textit{ecce}: that is, to also have a partial isomorphism between the
ADT and the visualization.  However, there is a challenge.  For other parts of
\textit{ecce}, the definition of partial isomorphisms on each transformation is
not obvious.  For example, it is not obvious how the inverse function of the
\textit{Event} constructor can be formulated.

As such, we have limited the application of partial isomorphisms purely to
syntax parsing and printing.
%endif

\begin{code}
un :: GlobalProtocol -> String
un e =
  case Text.Syntax.Printer.Naive.print parseGlobalProtocol e of
    Just x -> x
    Nothing -> error "Print error: " ++ show e
\end{code}
}

\slide{UnparserIntroduction}
\slide{UnparserUn}
