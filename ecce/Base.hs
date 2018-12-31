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
import Text.Parsec (ParseError, Parsec, parse)

type SParsec = Parsec String ()

extractParse :: SParsec a -> String -> Either ParseError a
extractParse p s = parse p "" s
