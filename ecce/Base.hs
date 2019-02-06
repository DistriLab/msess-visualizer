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
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, parse)

type SParsec = Parsec Void String

extractParse :: SParsec a -> String -> Either (ParseErrorBundle String Void) a
extractParse p s = parse p "" s
