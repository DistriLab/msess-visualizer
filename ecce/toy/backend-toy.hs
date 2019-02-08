{- SECTION PRAGMAS -}
-- Allows putting `deriving` on a standalone line, needed for GADTs to derive 
-- (Show)
{-# LANGUAGE StandaloneDeriving #-}
-- Allows constrained ASTs
{-# LANGUAGE GADTs #-}
-- Allows more than one StandaloneDeriving
{-# LANGUAGE UndecidableInstances #-}
-- Allows ambiguity check in instance declarations, to use sites
{-# LANGUAGE AllowAmbiguousTypes #-}
-- Allows datatypes without constructors
{-# LANGUAGE EmptyDataDecls #-}
-- Pragmas of invertible-parser
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction,
  RelaxedPolyRec #-}

{-
 - SECTION IMPORTS
 -}
import Control.Category ((.))
import Control.Isomorphism.Partial ((<$>), cons, inverse, right, subset)
import Control.Isomorphism.Partial.TH (defineIsomorphisms)
import Control.Isomorphism.Partial.Unsafe (Iso(..))
import Control.Monad
import Data.Char (isDigit, isLetter)
import Data.List (head, intercalate)
import Prelude
  ( Bool
  , Char
  , Eq(..)
  , IO
  , Integer
  , Maybe(..)
  , Show(..)
  , String
  , notElem
  , reads
  )
import System.IO (IOMode(ReadMode), hClose, hGetContents, openFile)
import Text.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Syntax
  ( Syntax
  , (*>)
  , (<*)
  , (<*>)
  , (<+>)
  , (<|>)
  , between
  , chainl1
  , many
  , optSpace
  , skipSpace
  , text
  , token
  )
import Text.Syntax.Parser.Naive (parse)
import Text.Syntax.Printer.Naive (print)

{-
 - SECTION USER INTERFACE
 -}
main = do
  inputLines <- interpreter
  let s = intercalate "\n" $ map (show . extractParse parseExpr) inputLines
   in putStrLn s

interpreter :: IO [String]
interpreter = do
  mapM_ putStrLn $
    "Either:" :
    "1) Type out expression" : "2) Load file (load <file>)" : "" : []
  inputLine <- getLine
  inputLines <-
    if take loadStringLength inputLine == loadString
      then do
        handle <- openFile (drop loadStringLength inputLine) ReadMode
        contents <- hGetContents handle
        seq (hClose handle) (return $ lines contents)
      else do
        return [inputLine]
  return inputLines
  where
    loadString = "load "
    loadStringLength = length loadString

{-
 - SECTION TYPES
 -}
type SParsec = Parsec String ()

data Pure

data Expr a where
  EBool :: Bool -> Expr Bool
  EPureBool :: Expr Bool -> Expr Pure
  EPureAnd :: Expr Pure -> Expr Pure -> Expr Pure
  EPureOr :: Expr Pure -> Expr Pure -> Expr Pure
  EPureNot :: Expr Pure -> Expr Pure
  EInteger :: Integer -> Expr Integer
  EIntegerNeg :: Expr Integer -> Expr Integer
  EIntegerMul :: Expr Integer -> Expr Integer -> Expr Integer
  EIntegerAdd :: Expr Integer -> Expr Integer -> Expr Integer

deriving instance Show (Expr a)

$(defineIsomorphisms ''Expr)

{-
 - SECTION LEXER
 -}
keywords = ["true", "false", "~", "^", "v"]

keywordOps = ["+", "-", "x", "^", "v", "~"]

letter, digit :: Syntax delta => delta Char
letter = subset isLetter <$> token

digit = subset isDigit <$> token

identifier =
  subset (`notElem` keywords) . cons <$> letter <*> many (letter <|> digit)

keyword :: Syntax delta => String -> delta ()
keyword s = inverse right <$> (identifier <+> text s)

integer :: Syntax delta => delta Integer
integer = Iso read' show' <$> many digit
  where
    read' s =
      case [x | (x, "") <- reads s] of
        [] -> Nothing
        (x:_) -> Just x
    show' x = Just (show x)

parens = between (text "(") (text ")")

ops = mulOp <$> text "*" <|> addOp <$> text "+"

spacedOps = between optSpace optSpace ops

{-
 - SECTION PARSERS
 -}
extractParse :: SParsec a -> String -> a
extractParse p s =
  case parse p "" s of
    Left x -> error $ show x
    Right x -> x

{-
 - SUBSECTION EXPR
 -}
parseExpr :: SParsec AnyExpr
parseExpr = do
  e <- try (anyExpr parsePure) <|> try (anyExpr parseInteger)
  return e

-- TODO priority should actually be defined only on Operators, not all Exprs.
priority :: Expr -> Integer
priority EIntegerNeg = 1
priority EIntegerAdd = 2
priority EIntegerMul = 3

expression =
  let t = keyword "true"
   in exp 2
  where
    exp 0 =
      literal <$> integer <|> variable <$> identifier <|> EBool <$> t <|>
      EBool <$> f <|>
      parens (skipSpace *> expression <* skipSpace)
    exp 1 = chainl1 (exp 0) spacedOps (binOpPrio 1)
    exp 2 = chainl1 (exp 1) spacedOps (binOpPrio 2)
                        {- TODO reference
		ifzero =
		  keyword "ifzero" *> optSpace *> parens (expression) <*> optSpace *>
		  parens (expression) <*>
		  optSpace *>
		  keyword "else" *>
		  optSpace *>
		  parens (expression)
		-}
    binOpPrio n = binOp . subset (\(x, (op, y)) -> priority op == n)
                -- f = keyword "false"

main = print expression (head (parse expression "false")) -- |true"))
