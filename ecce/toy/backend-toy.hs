{- SECTION PRAGMAS -}
-- Pragmas of invertible-parser
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

module Toy where

{-
 - SECTION IMPORTS
 -}
import Control.Category ((.))
import Control.Isomorphism.Partial ((<$>), cons, inverse, right, subset)
import Control.Isomorphism.Partial.TH (defineIsomorphisms)
import Control.Isomorphism.Partial.Unsafe (Iso(..))
import Data.Char (isDigit, isLetter)
import Data.List (head)
import Prelude
  ( Char
  , Eq(..)
  , Integer
  , Maybe(..)
  , Show(..)
  , String
  , notElem
  , reads
  )
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
data Expr
  = EBool
  | EInteger Integer
  | EOpUnary OpUnary
             Expr
  | EOpBinary Expr
              OpBinary
              Expr
  deriving (Show)

data OpUnary
  = EPureBool
  | EPureNot
  | EIntegerNeg
  deriving (Show, Eq)

data OpBinary
  = EPureAnd
  | EPureOr
  | EIntegerMul
  | EIntegerAdd
  deriving (Show, Eq)

$(defineIsomorphisms ''Expr)

$(defineIsomorphisms ''OpUnary)

$(defineIsomorphisms ''OpBinary)

{-
 - SECTION LEXER
 -}
{- TODO remove this -}
keywords = ["true", "false", "~", "^", "v"]

{- TODO remove this -}
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

ops = eIntegerMul <$> text "*" <|> eIntegerAdd <$> text "+"

spacedOps = between optSpace optSpace ops

priority :: OpBinary -> Integer
priority EIntegerMul = 1
priority EIntegerAdd = 2

expression = exp 2
  where
    exp 0 =
      eInteger <$> integer <|> eBool <$> keyword "true" <|>
      eBool <$> keyword "false" <|>
      parens (skipSpace *> expression <* skipSpace)
    exp 1 = chainl1 (exp 0) spacedOps (opPrioBinary 1)
    exp 2 = chainl1 (exp 1) spacedOps (opPrioBinary 2)
    opPrioBinary n = eOpBinary . subset (\(x, (op, y)) -> priority op == n)

main = print expression (head (parse expression "1+2"))
-- main = print expression (EOpBinary (EInteger 1) EIntegerAdd (EInteger 2))
