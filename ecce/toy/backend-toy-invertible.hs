{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction,
  RelaxedPolyRec #-}

module Evaluation where

import Control.Category
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH
import Control.Isomorphism.Partial.Unsafe
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
import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive

data Expression
  = Variable String
  | Literal Integer
  | BinOp Expression
          Operator
          Expression
  | IfZero Expression
           Expression
           Expression
  deriving (Show, Eq)

data Operator
  = AddOp
  | MulOp
  deriving (Show, Eq)

$(defineIsomorphisms ''Expression)

$(defineIsomorphisms ''Operator)

keywords = ["ifzero", "else"]

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

priority :: Operator -> Integer
priority MulOp = 1
priority AddOp = 2

expression = exp 2
  where
    exp 0 =
      literal <$> integer <|> variable <$> identifier <|> ifZero <$> ifzero <|>
      parens (skipSpace *> expression <* skipSpace)
    exp 1 = chainl1 (exp 0) spacedOps (binOpPrio 1)
    exp 2 = chainl1 (exp 1) spacedOps (binOpPrio 2)
    ifzero =
      keyword "ifzero" *> optSpace *> parens (expression) <*> optSpace *>
      parens (expression) <*>
      optSpace *>
      keyword "else" *>
      optSpace *>
      parens (expression)
    binOpPrio n = binOp . subset (\(x, (op, y)) -> priority op == n)

main = print expression (head (parse expression "ifzero (2+3*4) (5) else (6)"))
{-
  print
    expression
    (BinOp (BinOp (Literal 7) AddOp (Literal 8)) MulOp (Literal 9))
-}
