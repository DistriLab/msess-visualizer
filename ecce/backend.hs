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
  , between
  , char
  , letter
  , parse
  , sepBy
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

type DataStructure = String

type Pure = String

type VarType = String

type Pointer = Integer

type BoolInteger = Integer

data Expr a
  {- pred ::= p(root,v*) = Φ inv π -}
  {- Φ ::= VΔ -}
  {- Δ ::= ∃v*.k^π | Δ*Δ -}
  {- κ ::= emp | v↦d<v*> | p(v*) | κ*κ -}
      where
  EHeapEmp :: Expr Heap
  EHeapMap
    :: Expr VarFirst -> Expr DataStructure -> [Expr VarFirst] -> Expr Heap
  EHeapPointer :: [Expr VarFirst] -> Expr Heap
  EHeapSeparate :: Expr Heap -> Expr Heap -> Expr Heap
  EDataStructure :: DataStructure -> Expr DataStructure
  {- π ::= v:t | b | a | π^π | πvπ | ~π | ∃v.π | ∀v.π | γ -}
  EVarType :: VarType -> Expr VarType
  EPureVarType :: Expr VarFirst -> Expr VarType -> Expr Pure
  EPureBool :: Expr Bool -> Expr Pure
  EPureBoolInteger :: Expr BoolInteger -> Expr Pure
  EPureAnd :: Expr Pure -> Expr Pure -> Expr Pure
  EPureOr :: Expr Pure -> Expr Pure -> Expr Pure
  EPureNot :: Expr Pure -> Expr Pure
  EPureExists :: Expr VarFirst -> Expr Pure -> Expr Pure
  EPureForall :: Expr VarFirst -> Expr Pure -> Expr Pure
  EPurePointer :: Expr Pointer -> Expr Pure
  {- γ ::= v=v | v=null | v/=v | v/=null -}
  EPointerEq :: Expr VarFirst -> Expr VarFirst -> Expr Pointer
  EPointerNull :: Expr VarFirst -> Expr Pointer
  EPointerNeq :: Expr VarFirst -> Expr VarFirst -> Expr Pointer
  EPointerNNull :: Expr VarFirst -> Expr Pointer
  {- b ::= true | false | b=b -}
  EBool :: Bool -> Expr Bool
  EBoolEq :: Expr Bool -> Expr Bool -> Expr Bool
  {- a ::= s=s | s<=s -}
  EBoolIntegerEq :: Expr Integer -> Expr Integer -> Expr BoolInteger
  EBoolIntegerLeq :: Expr Integer -> Expr Integer -> Expr BoolInteger
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
    , Token.reservedNames = ["true", "false", "emp", "null"]
    , Token.reservedOpNames =
        [ "+"
        , "-"
        , "x"
        , "~"
        , "^"
        , "v"
        , "~"
        , "*"
        , "="
        , "<="
        , "/="
        , "="
        , "/="
        , ":"
        , "."
        , "E"
        , "A"
        ]
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
  parens parseHeap <|> parseHeapEmp <|> parseHeapMap <|> parseHeapPointer

parseHeapEmp :: SParsec (Expr Heap)
parseHeapEmp = reserved "emp" >> return EHeapEmp

parseHeapMap :: SParsec (Expr Heap)
parseHeapMap = do
  v1 <- parseVarFirst
  string "->"
  d <- liftM EDataStructure identifier
  vs <- between (char '<') (char '>') (sepBy parseVarFirst (char ','))
  return $ EHeapMap v1 d vs

parseHeapPointer :: SParsec (Expr Heap)
parseHeapPointer = do
  string "p("
  vs <- sepBy parseVarFirst (char ',')
  char ')'
  return $ EHeapPointer vs

{-
 - SUBSECTION π
 -}
parsePure :: SParsec (Expr Pure)
parsePure = buildExpressionParser opPure termPure

opPure =
  [ [Prefix (reservedOp "~" >> return EPureNot)]
  , [ Infix (reservedOp "^" >> return EPureAnd) AssocLeft
    , Infix (reservedOp "v" >> return EPureOr) AssocLeft
    ]
  ]

termPure =
  parens parsePure <|> try parsePureVarType <|> try parsePureBool <|>
  try parsePureBoolInteger <|>
  try parsePureExists <|>
  try parsePureForall <|>
  try parsePurePointer

parsePureVarType :: SParsec (Expr Pure)
parsePureVarType = do
  v <- parseVarFirst
  reservedOp ":"
  t <- parseVarType
  return $ EPureVarType v t

parseVarType :: SParsec (Expr VarType)
parseVarType = liftM EVarType identifier

parsePureBool :: SParsec (Expr Pure)
parsePureBool = do
  b <- parseBool
  return $ EPureBool b

parsePureBoolInteger :: SParsec (Expr Pure)
parsePureBoolInteger = do
  bi <- parseBoolInteger
  return $ EPureBoolInteger bi

parsePureExists :: SParsec (Expr Pure)
parsePureExists = do
  reservedOp "E"
  v <- parseVarFirst
  reservedOp "."
  p <- parsePure
  return $ EPureExists v p

parsePureForall :: SParsec (Expr Pure)
parsePureForall = do
  reservedOp "A"
  v <- parseVarFirst
  reservedOp "."
  p <- parsePure
  return $ EPureForall v p

parsePurePointer :: SParsec (Expr Pure)
parsePurePointer = do
  p <- parsePointer
  return $ EPurePointer p

{-
 - SUBSECTION γ
 -}
parsePointer :: SParsec (Expr Pointer)
parsePointer =
  try parsePointerEq <|> try parsePointerNull <|> try parsePointerNeq <|>
  try parsePointerNNull

parsePointerEq :: SParsec (Expr Pointer)
parsePointerEq = do
  v1 <- parseVarFirst
  char '='
  v2 <- parseVarFirst
  return $ EPointerEq v1 v2

parsePointerNull :: SParsec (Expr Pointer)
parsePointerNull = do
  v <- parseVarFirst
  reservedOp "="
  reserved "null"
  return $ EPointerNull v

parsePointerNeq :: SParsec (Expr Pointer)
parsePointerNeq = do
  v1 <- parseVarFirst
  reservedOp "/="
  v2 <- parseVarFirst
  return $ EPointerNeq v1 v2

parsePointerNNull :: SParsec (Expr Pointer)
parsePointerNNull = do
  v <- parseVarFirst
  reservedOp "/="
  reserved "null"
  return $ EPointerNNull v

{-
 - SUBSECTION b
 -}
parseBool :: SParsec (Expr Bool)
parseBool = buildExpressionParser opBool termBool

opBool = [[Infix (reservedOp "=" >> return EBoolEq) AssocLeft]]

termBool =
  parens parseBool <|> (reserved "true" >> return (EBool True)) <|>
  (reserved "false" >> return (EBool False))

{-
 - SUBSECTION a
 -}
parseBoolInteger :: SParsec (Expr BoolInteger)
parseBoolInteger = try parseBoolIntegerEq <|> try parseBoolIntegerLeq

parseBoolIntegerEq :: SParsec (Expr BoolInteger)
parseBoolIntegerEq = do
  s1 <- parseInteger
  reservedOp "="
  s2 <- parseInteger
  return $ EBoolIntegerEq s1 s2

parseBoolIntegerLeq :: SParsec (Expr BoolInteger)
parseBoolIntegerLeq = do
  s1 <- parseInteger
  reservedOp "<="
  s2 <- parseInteger
  return $ EBoolIntegerLeq s1 s2

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
parseVarFirst = liftM EVarFirst integer

{-
 - SUBSECTION PROTOCOL
 -}
{-
 - SUBSECTION EXPR
 -}
parseExpr :: SParsec AnyExpr
parseExpr = do
  e <-
    try (anyExpr parsePure) <|> try (anyExpr parsePointer) <|>
    try (anyExpr parseHeap) <|>
    try (anyExpr parseBoolInteger) <|>
    try (anyExpr parseInteger) <|>
    try (anyExpr parseBool)
  return e
