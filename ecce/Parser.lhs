\documentclass{article}
%include polycode.fmt
\begin{document}
\begin{code}
{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE StandaloneDeriving #-} -- Allows `deriving` on its own line
{-# LANGUAGE GADTs #-} -- Allows constrained ASTs
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns
{-# LANGUAGE EmptyDataDecls #-} -- Allows datatypes without constructors

{-
 - SECTION MODULE
 -}
module Parser where

{-
 - SECTION IMPORTS
 -}
import Base (SParsec, extractParse)
import Control.Exception (SomeException)
import qualified Control.Exception (try)
import Control.Monad (liftM)
import Data.List (intercalate)
import Interpreter (Output, mainHaskeline)
import System.IO (FilePath, readFile)
import Text.Parsec
  ( (<|>)
  , alphaNum
  , between
  , eof
  , lower
  , manyTill
  , optionMaybe
  , sepBy
  , sepBy1
  , try
  )
import Text.ParserCombinators.Parsec.Expr
  ( Assoc(AssocLeft)
  , Operator(Infix, Prefix)
  , buildExpressionParser
  )
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token

{-
 - SECTION USER INTERFACE
 -}
type Test = (Integer, String, String)

main :: IO ()
main = mainHaskeline commandOutputs incommandOutput

commandOutputs :: [(String, Output)]
commandOutputs =
  [ ( "help"
    , \(_, commands, _) ->
        mapM_ putStrLn $ "Here are a list of commands:" : commands)
  , ( "load"
    , \(_, _, restInputLine) -> parseFileLoad restInputLine >>= mapM_ putStrLn)
  , ( "test"
    , \(_, _, restInputLine) -> parseFileTest restInputLine >>= mapM_ putStrLn)
  ]

incommandOutput :: Output
incommandOutput = \(inputLine, _, _) -> putStrLn $ extractParseShow inputLine

-- Parse file at filePath with shower
parseFile ::
     Show a
  => FilePath
  -> String
  -> (a -> String)
  -> ([String] -> [a])
  -> IO [String]
parseFile filePath s shower f = do
  xs <- extractFile filePath
  return $ either (\e -> s : "Error: " : e) (\xs -> map shower (f xs)) xs

parseFileLoad :: FilePath -> IO [String]
parseFileLoad filePath =
  parseFile filePath "Usage:\n\tload <relativepath>" (extractParseShow) id

-- All test files must follow a strict format:
-- Must be in test/parser/ directory
-- File contents: must be even number of lines long
-- Each pair of lines is:
-- (1) Input expression
-- (2) Expected result of running (1)
parseFileTest :: FilePath -> IO [String]
parseFileTest filePath =
  parseFile
    ("test/parser/" ++ filePath)
    "Usage:\n\ttest filename\n\ttest list"
    parseTest
    (indexAndPair . splitEvenOdd)
  where
    splitEvenOdd :: [a] -> ([a], [a])
    splitEvenOdd = foldr (\x ~(xs2, xs1) -> (x : xs1, xs2)) ([], [])
    indexAndPair :: ([a], [a]) -> [(Integer, a, a)]
    indexAndPair = uncurry (zip3 [0 ..])

extractFile :: FilePath -> IO (Either [String] [String])
extractFile filePath = do
  xs <- (Control.Exception.try . fmap lines . readFile) filePath
  return $
    either (\(e :: SomeException) -> Left $ show e : []) (\xs -> Right xs) xs

parseTest :: Test -> String
parseTest (n, i, o) =
  if e == o
    then concat $ "#" : show n : ":\tP" : []
    else concat $
         "#" : show n : ":\tF\n\texpect\t" : o : "\n\tactual\t" : e : []
  where
    e = extractParseShow i

{-
 - SECTION TYPES
 -}
{-
 - SUBSECTION HELPERS
 -}
type VarFirst = Integer

type DataStructure = String

type VarType = String

type Predicate = String

type Role = String

type Channel = String

type Label = Integer

{- Figure 2.2 -}
data SymbolicPredicate

data FormulaDisjunct

data Formula

data Heap

data Pure

data Pointer

data BoolInteger

{- Figure 4.1 -}
data GlobalProtocol

{- Figure 4.3 -}
data Event

data Constraint

data Assertion

{- Figure 4.5 -}
data PartyProtocol

data EndpointProtocol

data ChannelProtocol

{- SUBSECTION EXPR -}
data Expr a
  {- HELPERS -}
      where
  EVarFirst :: VarFirst -> Expr VarFirst
  EDataStructure :: DataStructure -> Expr DataStructure
  EVarType :: VarType -> Expr VarType
  EPredicate :: Predicate -> Expr Predicate
  ERole :: Role -> Expr Role
  EChannel :: Channel -> Expr Channel
  ELabel :: Label -> Expr Label
  {- Figure 2.2 -}
  {- pred ::= p(root,v*) = Φ inv π -}
  ESymbolicPredicate
    :: Expr Predicate
    -> [AnyExpr]
    -> Expr FormulaDisjunct
    -> Expr Pure
    -> Expr SymbolicPredicate
  {- Φ ::= |Δ -}
  EFormulaDisjunct :: [Expr Formula] -> Expr FormulaDisjunct
  {- Δ ::= ∃v*.κ^π | Δ*Δ -}
  EFormulaExists :: [Expr VarFirst] -> Expr Heap -> Expr Pure -> Expr Formula
  EFormulaSeparate :: Expr Formula -> Expr Formula -> Expr Formula
  {- κ ::= emp | v↦d<v*> | p(v*) | κ*κ -}
  EHeapEmp :: Expr Heap
  EHeapMap
    :: Expr VarFirst -> Expr DataStructure -> [Expr VarFirst] -> Expr Heap
  EHeapPredicate :: Expr Predicate -> [AnyExpr] -> Expr Heap
  EHeapSeparate :: Expr Heap -> Expr Heap -> Expr Heap
  {- π ::= v:t | b | a | π^π | π|π | ~π | ∃v.π | ∀v.π | γ -}
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
  EPointerNEq :: Expr VarFirst -> Expr VarFirst -> Expr Pointer
  EPointerNNull :: Expr VarFirst -> Expr Pointer
  {- b ::= true | false | b=b -}
  EBool :: Bool -> Expr Bool
  EBoolEq :: Expr Bool -> Expr Bool -> Expr Bool
  {- a ::= s=s | s<=s -}
  EBoolIntegerEq :: Expr Integer -> Expr Integer -> Expr BoolInteger
  EBoolIntegerLeq :: Expr Integer -> Expr Integer -> Expr BoolInteger
  {- s ::= k | v | k x s | s + s | -s -}
  EInteger :: Integer -> Expr Integer
  EIntegerVarFirst :: Expr VarFirst -> Expr Integer
  EIntegerMul :: Expr Integer -> Expr Integer -> Expr Integer
  EIntegerAdd :: Expr Integer -> Expr Integer -> Expr Integer
  EIntegerNeg :: Expr Integer -> Expr Integer
  {- Figure 4.1 -}
  {- G ::= S--(i)->R:c<v.Δ> | G*G | G|G | G;G | (+)(Ψ) | (-)(Ψ) | emp -}
  EGlobalProtocolTransmission
    :: Expr Role
    -> Maybe (Expr Label)
    -> Expr Role
    -> Expr Channel
    -> Expr VarFirst
    -> Expr Formula
    -> Expr GlobalProtocol
  EGlobalProtocolConcurrency
    :: Expr GlobalProtocol -> Expr GlobalProtocol -> Expr GlobalProtocol
  EGlobalProtocolChoice
    :: Expr GlobalProtocol -> Expr GlobalProtocol -> Expr GlobalProtocol
  EGlobalProtocolSequencing
    :: Expr GlobalProtocol -> Expr GlobalProtocol -> Expr GlobalProtocol
  EGlobalProtocolAssumption :: Expr Assertion -> Expr GlobalProtocol
  EGlobalProtocolGuard :: Expr Assertion -> Expr GlobalProtocol
  EGlobalProtocolEmp :: Expr GlobalProtocol
  {- Figure 4.3 -}
  {- E ::= P(i) -}
  EEvent :: Expr Role -> Maybe (Expr Label) -> Expr Event
  {- ν ::= E<CBE | E<HBE -}
  EConstraintCommunicates :: Expr Event -> Expr Event -> Expr Constraint
  EConstraintHappens :: Expr Event -> Expr Event -> Expr Constraint
  {- Ψ ::= E | ~(E) | ν | Ψ^Ψ | E==>Ψ -}
  EAssertionEvent :: Expr Event -> Expr Assertion
  EAssertionNEvent :: Expr Event -> Expr Assertion
  EAssertionConstraint :: Expr Constraint -> Expr Assertion
  EAssertionAnd :: Expr Assertion -> Expr Assertion -> Expr Assertion
  EAssertionImplies :: Expr Event -> Expr Assertion -> Expr Assertion
  {- Figure 4.5 -}
  {- γ ::= c(i)!v.Δ | c(i)?v.Δ | γ*γ | γ|γ | γ;γ | (-)(Ψ) | (+)(Ψ) -}
  EPartyProtocolSend
    :: Expr Channel
    -> Maybe (Expr Label)
    -> Expr VarFirst
    -> Expr Formula
    -> Expr PartyProtocol
  EPartyProtocolReceive
    :: Expr Channel
    -> Maybe (Expr Label)
    -> Expr VarFirst
    -> Expr Formula
    -> Expr PartyProtocol
  EPartyProtocolConcurrency
    :: Expr PartyProtocol -> Expr PartyProtocol -> Expr PartyProtocol
  EPartyProtocolChoice
    :: Expr PartyProtocol -> Expr PartyProtocol -> Expr PartyProtocol
  EPartyProtocolSequencing
    :: Expr PartyProtocol -> Expr PartyProtocol -> Expr PartyProtocol
  EPartyProtocolAssumption :: Expr Assertion -> Expr PartyProtocol
  EPartyProtocolGuard :: Expr Assertion -> Expr PartyProtocol
  EPartyProtocolEmp :: Expr PartyProtocol
  {- L ::= (i)!v.Δ | (i)?v.Δ | L|L | L;L | (-)(Ψ) | (+)(Ψ) -}
  EEndpointProtocolSend
    :: Expr Channel
    -> Maybe (Expr Label)
    -> Expr VarFirst
    -> Expr Formula
    -> Expr EndpointProtocol
  EEndpointProtocolReceive
    :: Expr Channel
    -> Maybe (Expr Label)
    -> Expr VarFirst
    -> Expr Formula
    -> Expr EndpointProtocol
  -- Note:
  --    We also define L ::= L*L.
  --    See Projector.hs, SUBSECTION PER PARTY SPEC -> PER ENDPOINT SPEC Note
  --    for more details.
  EEndpointProtocolConcurrency
    :: Expr EndpointProtocol -> Expr EndpointProtocol -> Expr EndpointProtocol
  EEndpointProtocolChoice
    :: Expr EndpointProtocol -> Expr EndpointProtocol -> Expr EndpointProtocol
  EEndpointProtocolSequencing
    :: Expr EndpointProtocol -> Expr EndpointProtocol -> Expr EndpointProtocol
  EEndpointProtocolAssumption :: Expr Assertion -> Expr EndpointProtocol
  EEndpointProtocolGuard :: Expr Assertion -> Expr EndpointProtocol
  EEndpointProtocolEmp :: Expr EndpointProtocol
  {- Z ::= P--(i)->P:v.Δ | Z|Z | Z;Z | (-)(Ψ) | (+)(Ψ) -}
  EChannelProtocolTransmission
    :: Expr Role
    -> Maybe (Expr Label)
    -> Expr Role
    -> Expr VarFirst
    -> Expr Formula
    -> Expr ChannelProtocol
  EChannelProtocolChoice
    :: Expr ChannelProtocol -> Expr ChannelProtocol -> Expr ChannelProtocol
  EChannelProtocolSequencing
    :: Expr ChannelProtocol -> Expr ChannelProtocol -> Expr ChannelProtocol
  EChannelProtocolAssumption :: Expr Assertion -> Expr ChannelProtocol
  EChannelProtocolGuard :: Expr Assertion -> Expr ChannelProtocol
  EChannelProtocolEmp :: Expr ChannelProtocol

deriving instance Show (Expr a)

-- Existentially quantify Expr
-- Contains a well-formed Expr, but precise type of Expr is secret
data AnyExpr where
  AnyExpr :: Expr a -> AnyExpr

instance Show (AnyExpr) where
  show (AnyExpr e) = show e

anyExpr :: SParsec (Expr a) -> SParsec AnyExpr
anyExpr e = fmap AnyExpr e

{-
 - SECTION LEXER
 -}
languageDef =
  emptyDef
    { Token.commentStart = "{-"
    , Token.commentEnd = "-}"
    , Token.identStart = lower
    , Token.identLetter = alphaNum
    , Token.reservedNames = ["true", "false", "emp", "null"]
    , Token.reservedOpNames =
        [ "+"
        , "-"
        , "x"
        , "~"
        , "^"
        , "|"
        , "~"
        , "*"
        , "="
        , "<="
        , "/="
        , "="
        , "/="
        , ":"
        , ";"
        , "."
        , ","
        , "E"
        , "A"
        , "<CB"
        , "<HB"
        , "==>"
        , "Guard"
        , "Assumption"
        , "!"
        , "?"
        , "--"
        , "->"
        , "Inv"
        , "--->"
        ]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

angles = Token.angles lexer

integer = Token.integer lexer

semi = Token.semi lexer

whiteSpace = Token.whiteSpace lexer

{-
 - SECTION PARSERS
 -}
extractParseShow :: String -> String
extractParseShow s =
  either show (\x -> intercalate " " (map show x)) $
  extractParse (manyTill parseExpr eof) s

{-
 - SUBSECTION HELPERS
 -}
parseVarFirst = liftM EVarFirst integer

parseDataStructure = liftM EDataStructure identifier

parseVarType = liftM EVarType identifier

parsePredicate = liftM EPredicate identifier

parseRole = liftM ERole identifier

parseChannel = liftM EChannel identifier

parseLabel = liftM ELabel integer

{- Figure 2.2 -}
{-
 - SUBSECTION pred
 -}
parseSymbolicPredicate = do
  pr <- parsePredicate
  es <- parens (parseExpr `sepBy` (reservedOp ","))
  reservedOp "="
  fd <- parseFormulaDisjunct
  reservedOp "Inv"
  pu <- parsePure
  return $ ESymbolicPredicate pr es fd pu

{-
 - SUBSECTION Φ
 -}
-- Disjunct must have at least 2 formulas
parseFormulaDisjunct = do
  f <- parseFormula
  reservedOp "|"
  fs <- parseFormula `sepBy1` (reservedOp "|")
  return $ EFormulaDisjunct (f : fs)

{-
 - SUBSECTION Δ
 -}
parseFormula = buildExpressionParser opFormula termFormula

opFormula = [[Infix (reservedOp "*" >> return EFormulaSeparate) AssocLeft]]

termFormula = parens parseFormula <|> try parseFormulaExists

parseFormulaExists = do
  reservedOp "E"
  vs <- parseVarFirst `sepBy` (reservedOp ",")
  reservedOp "."
  h <- parseHeap
  reservedOp "^"
  p <- parsePure
  return $ EFormulaExists vs h p

{-
 - SUBSECTION κ
 -}
parseHeap = buildExpressionParser opHeap termHeap

opHeap = [[Infix (reservedOp "*" >> return EHeapSeparate) AssocLeft]]

termHeap =
  parens parseHeap <|> try parseHeapEmp <|> try parseHeapMap <|>
  try parseHeapPredicate

parseHeapEmp = reserved "emp" >> return EHeapEmp

parseHeapMap = do
  v1 <- parseVarFirst
  reservedOp "->"
  d <- parseDataStructure
  vs <- angles $ parseVarFirst `sepBy` (reservedOp ",")
  return $ EHeapMap v1 d vs

parseHeapPredicate = do
  p <- parsePredicate
  es <- parens $ parseExpr `sepBy` (reservedOp ",")
  return $ EHeapPredicate p es

{-
 - SUBSECTION π
 -}
parsePure = buildExpressionParser opPure termPure

opPure =
  [ [Prefix (reservedOp "~" >> return EPureNot)]
  , [ Infix (reservedOp "^" >> return EPureAnd) AssocLeft
    , Infix (reservedOp "|" >> return EPureOr) AssocLeft
    ]
  ]

termPure =
  parens parsePure <|> try parsePureVarType <|> try parsePureBool <|>
  try parsePureBoolInteger <|>
  try parsePureExists <|>
  try parsePureForall <|>
  try parsePurePointer

parsePureVarType = do
  v <- parseVarFirst
  reservedOp ":"
  t <- parseVarType
  return $ EPureVarType v t

parsePureBool = do
  b <- parseBool
  return $ EPureBool b

parsePureBoolInteger = do
  bi <- parseBoolInteger
  return $ EPureBoolInteger bi

parsePureExists = do
  reservedOp "E"
  v <- parseVarFirst
  reservedOp "."
  p <- parsePure
  return $ EPureExists v p

parsePureForall = do
  reservedOp "A"
  v <- parseVarFirst
  reservedOp "."
  p <- parsePure
  return $ EPureForall v p

parsePurePointer = do
  p <- parsePointer
  return $ EPurePointer p

{-
 - SUBSECTION γ
 -}
parsePointer =
  try parsePointerEq <|> try parsePointerNull <|> try parsePointerNEq <|>
  try parsePointerNNull

parsePointerEq = do
  v1 <- parseVarFirst
  reservedOp "="
  v2 <- parseVarFirst
  return $ EPointerEq v1 v2

parsePointerNull = do
  v <- parseVarFirst
  reservedOp "="
  reserved "null"
  return $ EPointerNull v

parsePointerNEq = do
  v1 <- parseVarFirst
  reservedOp "/="
  v2 <- parseVarFirst
  return $ EPointerNEq v1 v2

parsePointerNNull = do
  v <- parseVarFirst
  reservedOp "/="
  reserved "null"
  return $ EPointerNNull v

{-
 - SUBSECTION b
 -}
parseBool = buildExpressionParser opBool termBool

opBool = [[Infix (reservedOp "=" >> return EBoolEq) AssocLeft]]

termBool =
  parens parseBool <|> (reserved "true" >> return (EBool True)) <|>
  (reserved "false" >> return (EBool False))

{-
 - SUBSECTION a
 -}
parseBoolInteger = try parseBoolIntegerEq <|> try parseBoolIntegerLeq

parseBoolIntegerEq = do
  s1 <- parseInteger
  reservedOp "="
  s2 <- parseInteger
  return $ EBoolIntegerEq s1 s2

parseBoolIntegerLeq = do
  s1 <- parseInteger
  reservedOp "<="
  s2 <- parseInteger
  return $ EBoolIntegerLeq s1 s2

{-
 - SUBSECTION s
 -}
parseInteger = buildExpressionParser opInteger termInteger

opInteger =
  [ [Prefix (reservedOp "-" >> return EIntegerNeg)]
  , [Infix (reservedOp "x" >> return EIntegerMul) AssocLeft]
  , [Infix (reservedOp "+" >> return EIntegerAdd) AssocLeft]
  ]

termInteger =
  parens parseInteger <|> liftM EInteger integer <|> try parseIntegerVarFirst

parseIntegerVarFirst = do
  v <- parseVarFirst
  return $ EIntegerVarFirst v

{- Figure 4.1 -}
{-
 - SUBSECTION G
 -}
parseGlobalProtocol = buildExpressionParser opGlobalProtocol termGlobalProtocol

opGlobalProtocol =
  [ [ Infix (reservedOp "*" >> return EGlobalProtocolConcurrency) AssocLeft
    , Infix (reservedOp "|" >> return EGlobalProtocolChoice) AssocLeft
    , Infix (reservedOp ";" >> return EGlobalProtocolSequencing) AssocLeft
    ]
  ]

termGlobalProtocol =
  parens parseGlobalProtocol <|> try parseGlobalProtocolTransmission <|>
  try parseGlobalProtocolAssumption <|>
  try parseGlobalProtocolGuard <|>
  try parseGlobalProtocolEmp

parseGlobalProtocolTransmission = do
  s <- parseRole
  i <-
    try
      (between
         (reservedOp "--")
         (reservedOp "->")
         (optionMaybe $ parens parseLabel)) <|>
    try (reservedOp "--->" >> optionMaybe parseLabel) -- TODO hack to match types
  r <- parseRole
  reservedOp ":"
  c <- parseChannel
  (v, f) <-
    angles
      (do v <- parseVarFirst
          reservedOp "."
          f <- parseFormula
          return (v, f))
  return $ EGlobalProtocolTransmission s i r c v f

parseGlobalProtocolAssumption = do
  reservedOp "Assumption"
  a <- parens parseAssertion
  return $ EGlobalProtocolAssumption a

parseGlobalProtocolGuard = do
  reservedOp "Guard"
  a <- parens parseAssertion
  return $ EGlobalProtocolGuard a

parseGlobalProtocolEmp = reserved "emp" >> return EGlobalProtocolEmp

{- Figure 4.3 -}
{-
 - SUBSECTION E
 -}
parseEvent = do
  p <- parseRole
  i <- optionMaybe $ parens parseLabel
  return $ EEvent p i

{-
 - SUBSECTION ν
 -}
parseConstraint = try parseConstraintCommunicates <|> try parseConstraintHappens

parseConstraintCommunicates = do
  e1 <- parseEvent
  reservedOp "<CB"
  e2 <- parseEvent
  return $ EConstraintCommunicates e1 e2

parseConstraintHappens = do
  e1 <- parseEvent
  reservedOp "<HB"
  e2 <- parseEvent
  return $ EConstraintHappens e1 e2

{-
 - SUBSECTION Ψ
 -}
parseAssertion = buildExpressionParser opAssertion termAssertion

opAssertion = [[Infix (reservedOp "^" >> return EAssertionAnd) AssocLeft]]

termAssertion =
  parens parseAssertion <|> try parseAssertionImplies <|>
  try parseAssertionConstraint <|>
  try parseAssertionNEvent <|>
  try parseAssertionEvent

parseAssertionEvent = do
  e <- parseEvent
  return $ EAssertionEvent e

parseAssertionNEvent = do
  reservedOp "~"
  e <- parens parseEvent
  return $ EAssertionNEvent e

parseAssertionConstraint = do
  c <- parseConstraint
  return $ EAssertionConstraint c

parseAssertionImplies = do
  e <- parseEvent
  reservedOp "==>"
  a <- parseAssertion
  return $ EAssertionImplies e a

{- Figure 4.5 -}
{-
 - SUBSECTION γ
 -}
parsePartyProtocol = buildExpressionParser opPartyProtocol termPartyProtocol

opPartyProtocol =
  [ [ Infix (reservedOp "*" >> return EPartyProtocolConcurrency) AssocLeft
    , Infix (reservedOp "|" >> return EPartyProtocolChoice) AssocLeft
    , Infix (reservedOp ";" >> return EPartyProtocolSequencing) AssocLeft
    ]
  ]

termPartyProtocol =
  parens parsePartyProtocol <|> try parsePartyProtocolSend <|>
  try parsePartyProtocolReceive <|>
  try parsePartyProtocolGuard <|>
  try parsePartyProtocolAssumption <|>
  try parsePartyProtocolEmp

parsePartyProtocolSend = do
  c <- parseChannel
  i <- optionMaybe $ parens parseLabel
  reservedOp "!"
  reservedOp ":"
  v <- parseVarFirst
  reservedOp "."
  f <- parseFormula
  return $ EPartyProtocolSend c i v f

parsePartyProtocolReceive = do
  c <- parseChannel
  i <- optionMaybe $ parens parseLabel
  reservedOp "?"
  reservedOp ":"
  v <- parseVarFirst
  reservedOp "."
  f <- parseFormula
  return $ EPartyProtocolReceive c i v f

parsePartyProtocolGuard = do
  reservedOp "Guard"
  a <- parens parseAssertion
  return $ EPartyProtocolGuard a

parsePartyProtocolAssumption = do
  reservedOp "Assumption"
  a <- parens parseAssertion
  return $ EPartyProtocolAssumption a

parsePartyProtocolEmp = reserved "emp" >> return EPartyProtocolEmp

{-
 - SUBSECTION L
 -}
parseEndpointProtocol =
  buildExpressionParser opEndpointProtocol termEndpointProtocol

opEndpointProtocol =
  [ [ Infix (reservedOp "*" >> return EEndpointProtocolConcurrency) AssocLeft
    , Infix (reservedOp "|" >> return EEndpointProtocolChoice) AssocLeft
    , Infix (reservedOp ";" >> return EEndpointProtocolSequencing) AssocLeft
    ]
  ]

termEndpointProtocol =
  parens parseEndpointProtocol <|> try parseEndpointProtocolSend <|>
  try parseEndpointProtocolReceive <|>
  try parseEndpointProtocolGuard <|>
  try parseEndpointProtocolAssumption <|>
  try parseEndpointProtocolEmp

parseEndpointProtocolSend = do
  c <- parseChannel
  i <- optionMaybe $ parens parseLabel
  reservedOp "!"
  reservedOp ":"
  v <- parseVarFirst
  reservedOp "."
  f <- parseFormula
  return $ EEndpointProtocolSend c i v f

parseEndpointProtocolReceive = do
  c <- parseChannel
  i <- optionMaybe $ parens parseLabel
  reservedOp "?"
  reservedOp ":"
  v <- parseVarFirst
  reservedOp "."
  f <- parseFormula
  return $ EEndpointProtocolReceive c i v f

parseEndpointProtocolGuard = do
  reservedOp "Guard"
  a <- parens parseAssertion
  return $ EEndpointProtocolGuard a

parseEndpointProtocolAssumption = do
  reservedOp "Assumption"
  a <- parens parseAssertion
  return $ EEndpointProtocolAssumption a

parseEndpointProtocolEmp = reserved "emp" >> return EEndpointProtocolEmp

{-
 - SUBSECTION Z
 -}
parseChannelProtocol =
  buildExpressionParser opChannelProtocol termChannelProtocol

opChannelProtocol =
  [ [ Infix (reservedOp "|" >> return EChannelProtocolChoice) AssocLeft
    , Infix (reservedOp ";" >> return EChannelProtocolSequencing) AssocLeft
    ]
  ]

termChannelProtocol =
  parens parseChannelProtocol <|> try parseChannelProtocolTransmission <|>
  try parseChannelProtocolGuard <|>
  try parseChannelProtocolAssumption <|>
  try parseChannelProtocolEmp

parseChannelProtocolTransmission = do
  s <- parseRole
  i <-
    try
      (between
         (reservedOp "--")
         (reservedOp "->")
         (optionMaybe $ parens parseLabel)) <|>
    try (reservedOp "--->" >> optionMaybe parseLabel) -- TODO hack to match types
  r <- parseRole
  reservedOp ":"
  v <- parseVarFirst
  reservedOp "."
  f <- parseFormula
  return $ EChannelProtocolTransmission s i r v f

parseChannelProtocolGuard = do
  reservedOp "Guard"
  a <- parens parseAssertion
  return $ EChannelProtocolGuard a

parseChannelProtocolAssumption = do
  reservedOp "Assumption"
  a <- parens parseAssertion
  return $ EChannelProtocolAssumption a

parseChannelProtocolEmp = reserved "emp" >> return EChannelProtocolEmp

{-
 - SUBSECTION EXPR
 -}
parseExpr :: SParsec AnyExpr
parseExpr =
  let parseOrder =
        anyExpr parseConstraint :
        anyExpr parseAssertion :
        anyExpr parseEvent :
        anyExpr parseGlobalProtocol :
        anyExpr parseSymbolicPredicate :
        anyExpr parseFormulaDisjunct :
        anyExpr parseFormula :
        anyExpr parsePointer :
        anyExpr parseHeap :
        anyExpr parseBoolInteger :
        anyExpr parsePure : anyExpr parseInteger : anyExpr parseBool : []
      parseInOrder = foldl (\p p' -> p <|> try p') (try h) t
        where
          (h:t) = parseOrder
   in parseInOrder
\end{code}
\end{document}
