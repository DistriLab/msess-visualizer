{- SECTION PRAGMAS -}
{-# LANGUAGE StandaloneDeriving #-}
-- Allows putting `deriving` on a standalone line, needed for GADTs to derive 
-- (Show)
{-# LANGUAGE GADTs #-} -- Allows type equalities (a ~ Bool)

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
  handle <- openFile (drop loadStringLength inputLine) ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
   in seq (hClose handle) (return inputLines)
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
  EIf :: Expr Bool -> Expr a -> Expr a -> Expr a

deriving instance Show a => Show (Expr a)

{-
 - SECTION PARSERS
 -}
extractParse :: SParsec a -> String -> a
extractParse p s =
  case parse p "" s of
    Left x -> error $ show x
    Right x -> x

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

parseIf :: SParsec (Expr Bool)
parseIf = do
  string "if "
  b <- parseExpr
  string " then "
  e1 <- parseExpr
  string " else "
  e2 <- parseExpr
  return $ EIf b e1 e2

-- TODO too many `try`s
parseExpr :: SParsec (Expr Bool)
parseExpr = do
  e <-
    (try parseIf <|> try parseNot <|> try parseAnd <|> try parseOr <|>
     try parseBool)
  return e

{-
 - SECTION EVALUATORS
 -}
evalExpr :: Expr a -> a
evalExpr (EBool b) = b
evalExpr (ENot b) = not $ evalExpr b
evalExpr (EAnd b1 b2) = (evalExpr b1) && (evalExpr b2)
evalExpr (EOr b1 b2) = (evalExpr b1) || (evalExpr b2)
evalExpr (EIf b e1 e2) =
  if (evalExpr b)
    then (evalExpr e1)
    else (evalExpr e2)
