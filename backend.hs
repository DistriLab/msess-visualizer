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

type VarFirst = Int

type VarSecond = String

type Role = Int

type Chan = Int

type Label = Int

data Heap
  = Emp
  | Map VarFirst
        VarFirst
  | Separate Heap
             Heap
  | VarFirst
  | VarSecond
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
  {- π ::= v:t | b|a | π^π | πvπ | ~π | ∃v.π | ∀v.π | γ -}
      where
  ENot :: Expr Bool -> Expr Bool
  EAnd :: Expr Bool -> Expr Bool -> Expr Bool
  EOr :: Expr Bool -> Expr Bool -> Expr Bool
  {- γ ::= v=v | v=null | v/=v | v/=null -}
  EPointerEq :: Expr VarFirst -> Expr VarFirst -> Expr Bool
  EPointerNull :: Expr VarFirst -> Expr Bool
  EPointerDiseq :: Expr VarFirst -> Expr VarFirst -> Expr Bool
  EPointerNotNull :: Expr VarFirst -> Expr Bool
  {- b ::= true | false | b=b -}
  EBool :: Bool -> Expr Bool
  EBoolEq :: Expr Bool -> Expr Bool -> Expr Bool
  {- a ::= s=s | s<=s | TODO maybe V=Δ -}
  EIntEq :: Expr Int -> Expr Int -> Expr Bool
  EIntLeq :: Expr Int -> Expr Int -> Expr Bool
  {- s ::= k | v | k x s | s + s | -s -}
  EInt :: Int -> Expr Int
  EVarFirstInt :: Expr VarFirst -> Expr Int
  EMul :: Expr Int -> Expr Int -> Expr Int
  EAdd :: Expr Int -> Expr Int -> Expr Int
  ENeg :: Expr Int -> Expr Int
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
{-
 - SUBSECTION π
 -}
parseNot :: SParsec (Expr Bool)
parseNot = do
  char '~'
  b <- parseBool
  return $ ENot b

parseAnd :: SParsec (Expr Bool)
parseAnd = do
  b1 <- parseBool
  char '&'
  b2 <- parseBool
  return $ EAnd b1 b2

parseOr :: SParsec (Expr Bool)
parseOr = do
  b1 <- parseBool
  char '|'
  b2 <- parseBool
  return $ EOr b1 b2

{-
 - SUBSECTION γ
 -}
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
{-
 - SUBSECTION s
 -}
parseInt :: SParsec (Expr Int)
parseInt = do
  i <- many digit
  return $ EInt $ read i

parseVarFirstInt :: SParsec (Expr Int)
parseVarFirstInt = do
  i <- parseInt
  return $ EVarFirstInt i

parseMul :: SParsec (Expr Int)
parseMul = do
  i1 <- parseInt
  char 'x'
  i2 <- parseInt
  return $ EMul i1 i2

parseAdd :: SParsec (Expr Int)
parseAdd = do
  i1 <- parseInt
  char '+'
  i2 <- parseInt
  return $ EAdd i1 i2

parseNeg :: SParsec (Expr Int)
parseNeg = do
  char '-'
  i <- parseInt
  return $ ENeg i

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
