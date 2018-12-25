{- SECTION PRAGMAS -}
-- Allows putting `deriving` on a standalone line, needed for GADTs to derive 
-- (Show)
{-# LANGUAGE StandaloneDeriving #-}
-- Allows constrained ASTs
{-# LANGUAGE GADTs #-}
-- Allows more than one StandaloneDeriving
{-# LANGUAGE UndecidableInstances #-}
-- Allows deriving for empty data types
{-# LANGUAGE EmptyDataDeriving #-}

{-
 - SECTION IMPORTS
import Text.ParserCombinators.Parsec.Language
 -}
import Control.Monad (liftM)
import Data.List (intercalate)
import System.IO (IOMode(ReadMode), hClose, hGetContents, openFile)
import Text.Parsec
  ( Parsec
  , (<|>)
  , alphaNum
  , char
  , choice
  , letter
  , many
  , parse
  , string
  , try
  )
import Text.ParserCombinators.Parsec.Expr
  ( Assoc(AssocLeft)
  , Operator(Infix, Postfix, Prefix)
  , buildExpressionParser
  )
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token

{-
 - SECTION USER INTERFACE
 -}
main = do
  welcome
  interpreter

welcome :: IO ()
welcome = do
  mapM_ putStrLn $
    "Welcome!" :
    "Type at the prompt. Either:" :
    "1) Type out expression, or" : "2) Load file (load <file>)" : "" : []

interpreter :: IO ()
interpreter = do
  putStr "msess> "
  inputLine <- getLine
  inputLines <-
    if take loadStringLength inputLine == loadString
      then do
        handle <- openFile (drop loadStringLength inputLine) ReadMode
        contents <- hGetContents handle
        seq (hClose handle) (return $ lines contents)
      else do
        return [inputLine]
  let s = intercalate "\n" $ map (show . extractParse parseExpr) inputLines
   in putStrLn s
  interpreter
  where
    loadString = "load "
    loadStringLength = length loadString

{-
 - SECTION TYPES
 -}
type SParsec = Parsec String ()

type VarFirst = Integer

type Heap = String

data Expr a
  {- pred ::= p(root,v*) = Φ inv π -}
  {- Φ ::= VΔ -}
  {- Δ ::= ∃v*.k^π | Δ*Δ -}
  {- κ ::= emp | v↦d<v*> | p(v*) | κ*κ | V -}
      where
  EHeapEmp :: Expr Heap
  EHeapMap :: Expr VarFirst -> Expr VarFirst -> Expr Heap
  EHeapPointer :: Expr VarFirst -> Expr Heap
  EHeapSeparate :: Expr Heap -> Expr Heap -> Expr Heap
  {- π ::= v:t | b | a | π^π | πvπ | ~π | ∃v.π | ∀v.π | γ -}
  {- γ ::= v=v | v=null | v/=v | v/=null -}
  {- b ::= true | false | b=b -}
  EBool :: Bool -> Expr Bool
  EBoolEq :: Expr Bool -> Expr Bool -> Expr Bool
  {- a ::= s=s | s<=s | TODO maybe V=Δ -}
  {- s ::= k | v | k x s | s + s | -s -}
  EInteger :: Integer -> Expr Integer
  EVarFirst :: VarFirst -> Expr VarFirst
  EIntegerVarFirst :: Expr VarFirst -> Expr Integer
  EIntegerMul :: Expr Integer -> Expr Integer -> Expr Integer
  EIntegerAdd :: Expr Integer -> Expr Integer -> Expr Integer
  EIntegerNeg :: Expr Integer -> Expr Integer
  {- G ::= G*G | GVG | G;G -}

deriving instance Show (Expr a)

-- Existentially quantify Expr
-- Contains a well-formed Expr, but precise type of Expr is secret
data AnyExpr where
  AnyExpr :: Expr a -> AnyExpr

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
 - SUBSECTION pred
 -}
{-
 - SUBSECTION Φ
 -}
{-
 - SUBSECTION Δ
 -}
{-
 - SUBSECTION κ
 -}
parseHeap :: SParsec (Expr Heap)
parseHeap = buildExpressionParser opHeap termHeap

opHeap = [[Infix (reservedOp "*" >> return EHeapSeparate) AssocLeft]]

termHeap =
  parens parseHeap <|> (reserved "emp" >> return EHeapEmp) <|> parseHeapMap <|>
  parseHeapPointer

-- TODO does opTable allow for parsing different types?
-- TODO define user-defined data type d
parseHeapMap :: SParsec (Expr Heap)
parseHeapMap = do
  v1 <- parseVarFirst
  string "->"
  v2 <- parseVarFirst
  return $ EHeapMap v1 v2

parseHeapPointer :: SParsec (Expr Heap)
parseHeapPointer = do
  string "p("
  v <- parseVarFirst
  char ')'
  return $ EHeapPointer v

{-
 - SUBSECTION π
 -}
{-
 - SUBSECTION γ
 -}
{-
 - SUBSECTION b
 -}
parseBool :: SParsec (Expr Bool)
parseBool = buildExpressionParser opBool termBool

opBool = [[Infix (reservedOp "=" >> return (EBoolEq)) AssocLeft]]

termBool =
  parens parseBool <|> (reserved "true" >> return (EBool True)) <|>
  (reserved "false" >> return (EBool False))

{-
 - SUBSECTION a
 -}
{-
 - SUBSECTION s
 -}
parseInteger :: SParsec (Expr Integer)
parseInteger = buildExpressionParser opInteger termInteger

opInteger =
  [ [Prefix (reservedOp "-" >> return EIntegerNeg)]
  , [Infix (reservedOp "x" >> return EIntegerMul) AssocLeft]
  , [Infix (reservedOp "+" >> return EIntegerAdd) AssocLeft]
  ]

termInteger =
  parens parseInteger <|> liftM EInteger integer <|> parseIntegerVarFirst

parseIntegerVarFirst :: SParsec (Expr Integer)
parseIntegerVarFirst = do
  v <- parseVarFirst
  return $ EIntegerVarFirst v

-- Also define VarFirst
parseVarFirst :: SParsec (Expr VarFirst)
parseVarFirst = buildExpressionParser opVarFirst termVarFirst

opVarFirst = [[]]

termVarFirst = parens parseVarFirst <|> liftM EVarFirst integer

{-
 - SUBSECTION PROTOCOL
 -}
{-
 - SUBSECTION EXPR
 -}
parseExpr :: SParsec AnyExpr
parseExpr = do
  e <-
    try (anyExpr parseHeap) <|> try (anyExpr parseInteger) <|>
    try (anyExpr parseBool)
  return e
