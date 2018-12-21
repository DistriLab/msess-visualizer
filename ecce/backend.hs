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

import Data.List (intercalate)
import System.IO (IOMode(ReadMode), hClose, hGetContents, openFile)

{-
 - SECTION IMPORTS
 -}
import Text.Parsec

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

type Heap = String

type Pure = String

type Pointer = String

type BoolInt = Int

type VarType = String

type VarFirst = Int

type VarSecond = String

type Prot = String

type Role = Int

type Chan = Int

type Label = Int

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
  EVarType :: VarType -> Expr VarType
  EPureVarType :: Expr VarFirst -> Expr VarType -> Expr Pure
  EPureBool :: Expr Bool -> Expr Pure
  EPureBoolInt :: Expr BoolInt -> Expr Pure
  EPureAnd :: Expr Pure -> Expr Pure -> Expr Pure
  EPureOr :: Expr Pure -> Expr Pure -> Expr Pure
  EPureNot :: Expr Pure -> Expr Pure
  EPureExists :: Expr VarFirst -> Expr Pure -> Expr Pure
  EPureForall :: Expr VarFirst -> Expr Pure -> Expr Pure
  EPurePointer :: Expr Pointer -> Expr Pure
  {- γ ::= v=v | v=null | v/=v | v/=null -}
  EPointerEq :: Expr VarFirst -> Expr VarFirst -> Expr Pointer
  EPointerNull :: Expr VarFirst -> Expr Pointer
  EPointerDiseq :: Expr VarFirst -> Expr VarFirst -> Expr Pointer
  EPointerNotNull :: Expr VarFirst -> Expr Pointer
  {- b ::= true | false | b=b -}
  EBool :: Bool -> Expr Bool
  EBoolEq :: Expr Bool -> Expr Bool -> Expr Bool
  {- a ::= s=s | s<=s | TODO maybe V=Δ -}
  EBoolInt :: BoolInt -> Expr BoolInt
  EBoolIntEq :: Expr Int -> Expr Int -> Expr BoolInt
  EBoolIntLeq :: Expr Int -> Expr Int -> Expr BoolInt
  {- s ::= k | v | k x s | s + s | -s -}
  EInt :: Int -> Expr Int
  EVarFirst :: Expr VarFirst -> Expr Int
  EIntMul :: Expr Int -> Expr Int -> Expr Int
  EIntAdd :: Expr Int -> Expr Int -> Expr Int
  EIntNeg :: Expr Int -> Expr Int
  {- G ::= G*G | GVG | G;G -}
  EConcurrency :: Expr Prot -> Expr Prot -> Expr Prot
  EChoice :: Expr Prot -> Expr Prot -> Expr Prot
  ESequencing :: Expr Prot -> Expr Prot -> Expr Prot

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
parseHeap =
  choice [parseHeapEmp, parseHeapMap, parseHeapPointer, parseHeapSeparate]

parseHeapEmp :: SParsec (Expr Heap)
parseHeapEmp = do
  string "emp"
  return $ EHeapEmp

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
  string "*)"
  return $ EHeapPointer v

parseHeapSeparate :: SParsec (Expr Heap)
parseHeapSeparate = do
  p1 <- parseHeap
  char '*'
  p2 <- parseHeap
  return $ EHeapSeparate p1 p2

{-
 - SUBSECTION π
 -}
parsePure :: SParsec (Expr Pure)
parsePure =
  choice
    [ parseVarType
    , parsePureVarType
    , parsePureBool
    , parsePureBoolInt
    , parsePureAnd
    , parsePureOr
    , parsePureNot
    , parsePureExists
    , parsePureForall
    , parsePurePointer
    ]

-- Lift VarType into Expr VarType
parseVarType :: SParsec (Expr VarType)
parseVarType = do
  t <- many alphaNum
  return $ EVarType t

parsePureVarType :: SParsec (Expr Pure)
parsePureVarType = do
  v <- parseVarFirst
  char ':'
  t <- parseVarType
  return $ EPureVarType v t

parsePureBool :: SParsec (Expr Pure)
parsePureBool = do
  b <- parseBool
  return $ EPureBool b

parsePureBoolInt :: SParsec (Expr Pure)
parsePureBoolInt = do
  bi <- parseBoolInt
  return $ EPureBoolInt bi

parsePureAnd :: SParsec (Expr Pure)
parsePureAnd = do
  p1 <- parsePure
  char '&'
  p2 <- parsePure
  return $ EPureAnd p1 p2

parsePureOr :: SParsec (Expr Pure)
parsePureOr = do
  p1 <- parsePure
  char '|'
  p2 <- parsePure
  return $ EPureOr p1 p2

parsePureNot :: SParsec (Expr Pure)
parsePureNot = do
  char '~'
  p <- parsePure
  return $ EPureNot p

parsePureExists :: SParsec (Expr Pure)
parsePureExists = do
  v <- parseVarFirst
  p <- parsePure
  return $ EPureExists v p

parsePureForall :: SParsec (Expr Pure)
parsePureForall = do
  v <- parseVarFirst
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
  choice
    [parsePointerEq, parsePointerNull, parsePointerDiseq, parsePointerNotNull]

parsePointerEq :: SParsec (Expr Pointer)
parsePointerEq = do
  v1 <- parseVarFirst
  char '='
  v2 <- parseVarFirst
  return $ EPointerEq v1 v2

parsePointerNull :: SParsec (Expr Pointer)
parsePointerNull = do
  v <- parseVarFirst
  string "=null"
  return $ EPointerNull v

parsePointerDiseq :: SParsec (Expr Pointer)
parsePointerDiseq = do
  v1 <- parseVarFirst
  string "/="
  v2 <- parseVarFirst
  return $ EPointerEq v1 v2

parsePointerNotNull :: SParsec (Expr Pointer)
parsePointerNotNull = do
  v <- parseVarFirst
  string "/=null"
  return $ EPointerNull v

{-
 - SUBSECTION b
 -}
parseBool :: SParsec (Expr Bool)
parseBool = do
  b <-
    (do string "True"
        return $ EBool True) <|>
    (do string "False"
        return $ EBool False)
  return b

parseBoolEq :: SParsec (Expr Bool)
parseBoolEq = do
  b1 <- parseBool
  char '='
  b2 <- parseBool
  return $ EBoolEq b1 b2

{-
 - SUBSECTION a
 -}
parseBoolInt :: SParsec (Expr BoolInt)
parseBoolInt = do
  choice [parseBoolIntEq, parseBoolIntLeq]

parseBoolIntEq :: SParsec (Expr BoolInt)
parseBoolIntEq = do
  i1 <- parseInt
  char '='
  i2 <- parseInt
  return $ EBoolIntEq i1 i2

parseBoolIntLeq :: SParsec (Expr BoolInt)
parseBoolIntLeq = do
  i1 <- parseInt
  string "<="
  i2 <- parseInt
  return $ EBoolIntLeq i1 i2

{-
 - SUBSECTION s
 -}
parseInt :: SParsec (Expr Int)
parseInt = do
  i <- many digit
  return $ EInt $ read i

parseVarFirst :: SParsec (Expr Int)
parseVarFirst = do
  i <- parseInt
  return $ EVarFirst i

parseIntMul :: SParsec (Expr Int)
parseIntMul = do
  i1 <- parseInt
  char 'x'
  i2 <- parseInt
  return $ EIntMul i1 i2

parseIntAdd :: SParsec (Expr Int)
parseIntAdd = do
  i1 <- parseInt
  char '+'
  i2 <- parseInt
  return $ EIntAdd i1 i2

parseIntNeg :: SParsec (Expr Int)
parseIntNeg = do
  char '-'
  i <- parseInt
  return $ EIntNeg i

{-
 - SUBSECTION PROTOCOL
 -}
{-
 - SUBSECTION EXPR
 -}
parseExpr :: SParsec AnyExpr
parseExpr = do
  e <- try (anyExpr parseBool) <|> try (anyExpr parseInt)
  return e
