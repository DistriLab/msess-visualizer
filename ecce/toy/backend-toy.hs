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

{-
 - SECTION IMPORTS
 -}
import Control.Monad
import Data.List (intercalate)
import System.IO (IOMode(ReadMode), hClose, hGetContents, openFile)
import Text.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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

{-
TyConI
  (DataD
     []
     Main.Expr
     [KindedTV a_6989586621679078654 StarT]
     Nothing
     [ GadtC
         [Main.EBool]
         [(Bang NoSourceUnpackedness NoSourceStrictness, ConT GHC.Types.Bool)]
         (AppT (ConT Main.Expr) (ConT GHC.Types.Bool))
     , GadtC
         [Main.EPureBool]
         [ ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT GHC.Types.Bool))
         ]
         (AppT (ConT Main.Expr) (ConT Main.Pure))
     , GadtC
         [Main.EPureAnd]
         [ ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT Main.Pure))
         , ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT Main.Pure))
         ]
         (AppT (ConT Main.Expr) (ConT Main.Pure))
     , GadtC
         [Main.EPureOr]
         [ ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT Main.Pure))
         , ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT Main.Pure))
         ]
         (AppT (ConT Main.Expr) (ConT Main.Pure))
     , GadtC
         [Main.EPureNot]
         [ ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT Main.Pure))
         ]
         (AppT (ConT Main.Expr) (ConT Main.Pure))
     , GadtC
         [Main.EInteger]
         [ ( Bang NoSourceUnpackedness NoSourceStrictness
           , ConT GHC.Integer.Type.Integer)
         ]
         (AppT (ConT Main.Expr) (ConT GHC.Integer.Type.Integer))
     , GadtC
         [Main.EIntegerNeg]
         [ ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT GHC.Integer.Type.Integer))
         ]
         (AppT (ConT Main.Expr) (ConT GHC.Integer.Type.Integer))
     , GadtC
         [Main.EIntegerMul]
         [ ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT GHC.Integer.Type.Integer))
         , ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT GHC.Integer.Type.Integer))
         ]
         (AppT (ConT Main.Expr) (ConT GHC.Integer.Type.Integer))
     , GadtC
         [Main.EIntegerAdd]
         [ ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT GHC.Integer.Type.Integer))
         , ( Bang NoSourceUnpackedness NoSourceStrictness
           , AppT (ConT Main.Expr) (ConT GHC.Integer.Type.Integer))
         ]
         (AppT (ConT Main.Expr) (ConT GHC.Integer.Type.Integer))
     ]
     [])
-}
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

-- Existentially quantify Expr
-- Contains a well-formed Expr, but precise type of Expr is secret
data AnyExpr where
  AnyExpr :: Expr a -> AnyExpr

deriving instance Show (Expr a)

instance Show (AnyExpr) where
  show (AnyExpr a) = show a

anyExpr :: SParsec (Expr a) -> SParsec AnyExpr
anyExpr e = fmap AnyExpr e

{-
 - SECTION LEXER
 -}
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames = ["true", "false", "~", "^", "v"]
    , Token.reservedOpNames = ["+", "-", "x", "^", "v", "~"]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier

reserved = Token.reserved lexer -- parses a reserved name

reservedOp = Token.reservedOp lexer -- parses an operator

parens = Token.parens lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them

integer = Token.integer lexer -- parses an integer

semi = Token.semi lexer -- parses a semicolon

whiteSpace = Token.whiteSpace lexer -- parses whitespace

{-
 - SECTION PARSERS
 -}
extractParse :: SParsec a -> String -> a
extractParse p s =
  case parse p "" s of
    Left x -> error $ show x
    Right x -> x

{-
 - SUBSECTION PURE
 -}
parsePure = buildExpressionParser opPure termPure

opPure =
  [ [Prefix (reservedOp "~" >> return EPureNot)]
  , [ Infix (reservedOp "^" >> return EPureAnd) AssocLeft
    , Infix (reservedOp "|" >> return EPureOr) AssocLeft
    ]
  ]

termPure = parens parsePure <|> try parsePureBool

parsePureBool = do
  b <- parseBool
  return $ EPureBool b

{-
 - SUBSECTION BOOL
 -}
parseBool =
  (reserved "true" >> return (EBool True)) <|>
  (reserved "false" >> return (EBool False))

{-
 - SUBSECTION INT
 -}
parseInteger = buildExpressionParser opInteger termInteger

opInteger =
  [ [Prefix (reservedOp "-" >> return EIntegerNeg)]
  , [Infix (reservedOp "x" >> return EIntegerMul) AssocLeft]
  , [Infix (reservedOp "+" >> return EIntegerAdd) AssocLeft]
  ]

termInteger = parens parseInteger <|> liftM EInteger integer

{-
 - SUBSECTION EXPR
 -}
parseExpr :: SParsec AnyExpr
parseExpr = do
  e <- try (anyExpr parsePure) <|> try (anyExpr parseInteger)
  return e
