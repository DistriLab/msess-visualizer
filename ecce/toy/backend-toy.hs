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
  ( Bool
  , Char
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
  = EBool Bool
  | EPureBool Expr
  | EInteger Integer
  | EOpUnary OpUnary
             Expr
  | EOpBinary Expr
              OpBinary
              Expr
  deriving (Show)

data OpUnary
  = EPureNot
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

opBoolUnary = ePureNot <$> text "~"

opBoolBinary = ePureAnd <$> text "^" <|> ePureOr <$> text "v"

opIntegerUnary :: Syntax delta => delta OpUnary
opIntegerUnary = eIntegerNeg <$> text "-"

opIntegerBinary :: Syntax delta => delta OpBinary
opIntegerBinary = eIntegerMul <$> text "*" <|> eIntegerAdd <$> text "+"

spacedOpIntegerBinary :: Syntax delta => delta OpBinary
spacedOpIntegerBinary = between optSpace optSpace opIntegerBinary

prioBinary :: OpBinary -> Integer
prioBinary EIntegerMul = 1
prioBinary EIntegerAdd = 2

expression = exp 2
  where
    exp 0 =
      eOpUnary <$> (opIntegerUnary <*> (eInteger <$> integer)) <|>
      eInteger <$> integer <|>
      parens (skipSpace *> expression <* skipSpace)
    exp 1 = chainl1 (exp 0) spacedOpIntegerBinary (opPrioBinary 1)
    exp 2 = chainl1 (exp 1) spacedOpIntegerBinary (opPrioBinary 2)
    opPrioBinary n = eOpBinary . subset (\(_, (op, _)) -> prioBinary op == n)

main = print expression (head (parse expression "-1+-2"))
{-
main =
  print
    expression
    (EOpBinary
       (EOpUnary EIntegerNeg (EInteger 1))
       EIntegerAdd
       (EOpUnary EIntegerNeg (EInteger 2)))
-}
