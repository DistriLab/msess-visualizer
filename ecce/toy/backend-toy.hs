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
import Data.List (elem, head)
import Prelude
  ( Bool(False, True)
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
keywords = []

letter, digit :: Syntax delta => delta Char
letter = subset isLetter <$> token

digit = subset isDigit <$> token

identifier :: Syntax delta => delta String
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

bool :: Syntax delta => delta Bool
bool = caster <$> (subset (`elem` ["true", "false"]) <$> many letter)
  where
    caster :: Iso String Bool
    caster =
      Iso
        (\s ->
           case s of
             "true" -> Just True
             "false" -> Just False)
        (\b ->
           case b of
             True -> Just "true"
             False -> Just "false")

parens = between (text "(") (text ")")

opPureUnary :: Syntax delta => delta OpUnary
opPureUnary = ePureNot <$> text "~"

opPureBinary :: Syntax delta => delta OpBinary
opPureBinary = ePureAnd <$> text "^" <|> ePureOr <$> text "v"

spacedOpPureBinary :: Syntax delta => delta OpBinary
spacedOpPureBinary = between optSpace optSpace opPureBinary

prioPureBinary :: OpBinary -> Integer
prioPureBinary EPureAnd = 1
prioPureBinary EPureOr = 2

opIntegerUnary :: Syntax delta => delta OpUnary
opIntegerUnary = eIntegerNeg <$> text "-"

opIntegerBinary :: Syntax delta => delta OpBinary
opIntegerBinary = eIntegerMul <$> text "*" <|> eIntegerAdd <$> text "+"

spacedOpIntegerBinary :: Syntax delta => delta OpBinary
spacedOpIntegerBinary = between optSpace optSpace opIntegerBinary

prioIntegerBinary :: OpBinary -> Integer
prioIntegerBinary EIntegerMul = 1
prioIntegerBinary EIntegerAdd = 2

exprInteger = exp 2
  where
    exp 0 =
      eOpUnary <$> (opIntegerUnary <*> (eInteger <$> integer)) <|>
      eInteger <$> integer <|>
      parens (skipSpace *> exprInteger <* skipSpace)
    exp 1 = chainl1 (exp 0) spacedOpIntegerBinary (opPrioIntegerBinary 1)
    exp 2 = chainl1 (exp 1) spacedOpIntegerBinary (opPrioIntegerBinary 2)
    opPrioIntegerBinary n =
      eOpBinary . subset (\(_, (op, _)) -> prioIntegerBinary op == n)

exprPure = exp 2
  where
    exp 0 =
      eOpUnary <$> (opPureUnary <*> (ePureBool <$> (eBool <$> bool))) <|>
      ePureBool <$> (eBool <$> bool) <|>
      parens (skipSpace *> exprPure <* skipSpace)
    exp 1 = chainl1 (exp 0) spacedOpPureBinary (opPrioPureBinary 1)
    exp 2 = chainl1 (exp 1) spacedOpPureBinary (opPrioPureBinary 2)
    opPrioPureBinary n =
      eOpBinary . subset (\(_, (op, _)) -> prioPureBinary op == n)

main = print exprPure (head (parse exprPure "~true ^ false"))
{-
main =
  print
    exprPure
    (EOpBinary
       (EOpUnary EPureNot (EPureBool (EBool True)))
       EPureAnd
       (EPureBool (EBool False)))
-}
-- main = print exprInteger (head (parse exprInteger "-1+-2"))
{-
main =
  print
    exprInteger
    (EOpBinary
       (EOpUnary EIntegerNeg (EInteger 1))
       EIntegerAdd
       (EOpUnary EIntegerNeg (EInteger 2)))
-}
