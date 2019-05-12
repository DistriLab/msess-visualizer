\subsection{Unparser}

The unparser, or pretty-printer, is just the inverse function of the parser.
That is, an unparser converts an ADT into a string.  Implementing this with
|partial-isomorphism|s is not only straightforward, but also has the advantage
of being isomorphic.  That is, an unparsed ADT can be parsed to get the same
ADT, and a parsed string can be unparsed to get the same string.

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
|parseGlobalProtocol| and a |GlobalProtocol| to |invertible-syntax|'s |print|
function.  We then unwrap the |Maybe| result, and display an error if |Nothing|
is returned.
\par
|un| takes a |GlobalProtocol| as input instead of any well-formed expression in
typical pretty-printers.  This is because partial isomorphisms and existential
types do not mix well.  It is possible to wrap a existential type |AnyExpr|
around all expression types (e.g. |Pure|, |GlobalProtocol|, etc...).  However,
deconstructing |AnyExpr| is impossible, since we do not know which expression
type is actually being wrapped inside |AnyExpr|.  Therefore, we can only
construct existential types, but not destruct existential types.  So, we are
unable to establish an isomorphism between the existential and the expression
types.
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
within |ecce|: that is, to also have a partial isomorphism between the ADT and
the visualization.  However, there is a challenge.  For other parts of |ecce|,
the definition of partial isomorphisms on each transformation is not obvious.
For example, it is not obvious how the inverse function of the |Event|
constructor can be formulated.
\par
As such, we have limited the application of partial isomorphisms purely to
syntax parsing and printing.

\begin{code}
un :: GlobalProtocol -> String
un e =
  case Text.Syntax.Printer.Naive.print parseGlobalProtocol e of
    Just x -> x
    Nothing -> error "Print error: " ++ show e
\end{code}
