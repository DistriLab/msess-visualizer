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

type VarFirst = Int

type VarType = String

type VarSecond = String

type Role = Int

type Chan = Int

type Label = Int

data Heap
  deriving (Show)

data Pure
  deriving (Show)

data Pointer
  deriving (Show)

data BoolInt
  deriving (Show)

data Prot
  = ProtTrans
  | ProtGuard Prot
              Prot
  | ProtAssume Prot
               Prot
  | ProtEmp Prot
            Prot
  deriving (Show)

data Expr a
  {- pred ::= p(root,v*) = Φ inv π -}
  {- Φ ::= VΔ -}
  {- Δ ::= ∃v*.k^π | Δ*Δ -}
  {- κ ::= emp | v↦d<v*> | p(v*) | κ*κ | V -}
      where
  EHeap :: Heap -> Expr Heap
  EHeapEmp :: Expr Heap -> Expr Heap
  EHeapMap :: Expr Pointer -> Expr Pointer -> Expr Heap
  EHeapPointer :: Expr Pointer -> Expr Heap
  EHeapSeparate :: Expr Heap -> Expr Heap -> Expr Heap
  {- π ::= v:t | b|a | π^π | πvπ | ~π | ∃v.π | ∀v.π | γ -}
  EPure :: Pure -> Expr Pure
  EPureType :: Expr VarFirst -> Expr VarType -> Expr Pure
  EPureNot :: Expr Pure -> Expr Pure
  EPureAnd :: Expr Pure -> Expr Pure -> Expr Pure
  EPureOr :: Expr Pure -> Expr Pure -> Expr Pure
  {- γ ::= v=v | v=null | v/=v | v/=null -}
  EPointer :: Pointer -> Expr Pointer
  EPointerEq :: Expr VarFirst -> Expr VarFirst -> Expr Pointer
  EPointerNull :: Expr VarFirst -> Expr Pointer
  EPointerDiseq :: Expr VarFirst -> Expr VarFirst -> Expr Pointer
  EPointerNotNull :: Expr VarFirst -> Expr Pointer
  {- b ::= true | false | b=b -}
  EBool :: Bool -> Expr Bool
  EBoolEq :: Expr Bool -> Expr Bool -> Expr Bool
  {- a ::= s=s | s<=s | TODO maybe V=Δ -}
  EBoolInt :: Bool -> Expr BoolInt
  EBoolIntEq :: Expr Int -> Expr Int -> Expr BoolInt
  EBoolIntLeq :: Expr Int -> Expr Int -> Expr BoolInt
  {- s ::= k | v | k x s | s + s | -s -}
  EInt :: Int -> Expr Int
  EIntVarFirst :: Expr VarFirst -> Expr Int
  EIntMul :: Expr Int -> Expr Int -> Expr Int
  EIntAdd :: Expr Int -> Expr Int -> Expr Int
  EIntNeg :: Expr Int -> Expr Int
  {- G ::= G*G | GVG | G;G -}
  EProt :: Prot -> Expr Prot
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
parseHeap = do
  p <- many alphaNum
  return $ EHeap p

parseHeapEmp :: SParsec (Expr Heap)
parseHeapEmp = do
  p <- parseHeap
  return $ EHeapEmp p

parseHeapMap :: SParsec (Expr Heap)
parseHeapMap = do
  p1 <- parseHeap
  char '&'
  p2 <- parseHeap
  return $ EHeapMap p1 p2

parseHeapPointer :: SParsec (Expr Heap)
parseHeapPointer = do
  p <- parsePointer
  char '~'
  p <- parseHeap
  return $ EHeapPointer p

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
parsePure = do
  p <- many alphaNum
  return $ EPure p

parsePureType :: SParsec (Expr Pure)
parsePureType = do
  p <- parsePure
  return $ EPureNot p

parsePureNot :: SParsec (Expr Pure)
parsePureNot = do
  char '~'
  p <- parsePure
  return $ EPureNot p

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

{-
 - SUBSECTION γ
 -}
parsePointer :: SParsec (Expr Pointer)
parsePointer = do
  v <- parseIntVarFirst
  return $ EPointer v

parsePointerEq :: SParsec (Expr Pointer)
parsePointerEq = do
  p1 <- parseparsePointer
  p2 <- parseparsePointer
  return $ EPointerEq p1 p2

parsePointerNull :: SParsec (Expr Pointer)
parsePointerNull = do
  p <- parseparsePointer
  return $ EPointerNull p

parsePointerDiseq :: SParsec (Expr Pointer)
parsePointerDiseq = do
  p1 <- parseparsePointer
  p2 <- parseparsePointer
  return $ EPointerEq p1 p2

parsePointerNotNull :: SParsec (Expr Pointer)
parsePointerNotNull = do
  p <- parseparsePointer
  return $ EPointerNull p

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
  b <-
    (do string "True"
        return $ EBoolInt True) <|>
    (do string "False"
        return $ EBoolInt False)
  return b

parseBoolIntEq :: SParsec (Expr BoolInt)
parseBoolIntEq = do
  b1 <- parseBoolInt
  char '='
  b2 <- parseBoolInt
  return $ EBoolIntEq b1 b2

parseBoolIntLeq :: SParsec (Expr BoolInt)
parseBoolIntLeq = do
  b1 <- parseBoolInt
  string "<="
  b2 <- parseBoolInt
  return $ EBoolIntLeq b1 b2

{-
 - SUBSECTION s
 -}
parseInt :: SParsec (Expr Int)
parseInt = do
  i <- many digit
  return $ EInt $ read i

parseIntVarFirst :: SParsec (Expr Int)
parseIntVarFirst = do
  i <- parseInt
  return $ EIntVarFirst i

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
  e <-
    try (anyExpr parseNot) <|> try (anyExpr parseAnd) <|> try (anyExpr parseOr) <|>
    try (anyExpr parseBool) <|>
    try (anyExpr parseAdd) <|>
    try (anyExpr parseInt)
  return e
