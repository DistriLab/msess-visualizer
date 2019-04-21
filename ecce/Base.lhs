\begin{code}
{-
 - Types or functions common to many modules
 -}
{-
 - SECTION MODULE
 -}
module Base where

{-
 - SECTION IMPORTS
 -}
import Text.Parsec (Parsec)
import Text.Syntax.Parser.Naive (Parser, parse)

type SParsec = Parsec String ()

extractParse :: Parser a -> String -> [a]
extractParse p s = parse p s
\end{code}
