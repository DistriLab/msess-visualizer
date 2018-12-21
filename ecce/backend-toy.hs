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

data Expr a where
  EBool :: Bool -> Expr Bool
  ENot :: Expr Bool -> Expr Bool
  EAnd :: Expr Bool -> Expr Bool -> Expr Bool
  EOr :: Expr Bool -> Expr Bool -> Expr Bool
  EInt :: Int -> Expr Int
  EAdd :: Expr Int -> Expr Int -> Expr Int

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
 - SUBSECTION BOOL
 -}
parseBool :: SParsec (Expr Bool)
parseBool = do
  b <-
    (do string "True"
        return $ EBool True) <|>
    (do string "False"
        return $ EBool False)
  return b

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
 - SUBSECTION INT
 -}
parseInt :: SParsec (Expr Int)
parseInt = do
  i <- many digit
  return $ EInt $ read i

parseAdd :: SParsec (Expr Int)
parseAdd = do
  i1 <- parseInt
  char '+'
  i2 <- parseInt
  return $ EAdd i1 i2

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
