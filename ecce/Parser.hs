{-
 - SECTION PRAGMAS
 -}
-- Pragmas of invertible-parser
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-} -- Allows `deriving` on its own line
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns

{-
 - SECTION MODULE
 -}
module Parser where

{-
 - SECTION IMPORTS
 -}
import Base (extractParse)
import Control.Category ((.))
import Control.Exception (SomeException)
import qualified Control.Exception (try)
import Control.Isomorphism.Partial ((<$>), cons, inverse, right, subset)
import Control.Isomorphism.Partial.TH (defineIsomorphisms)
import Control.Isomorphism.Partial.Unsafe (Iso(..))
import Data.Char (isDigit, isLetter)
import Data.List (elem, head)
import Interpreter (Output, mainHaskeline)
import Prelude
  ( Bool(False, True)
  , Char
  , Either(..)
  , Eq(..)
  , IO
  , Integer
  , Maybe(..)
  , Show(..)
  , String
  , ($)
  , (++)
  , (>>=)
  , concat
  , either
  , fmap
  , foldr
  , id
  , lines
  , map
  , mapM_
  , notElem
  , putStrLn
  , reads
  , return
  , uncurry
  , zip3
  )
import System.IO (FilePath, readFile)
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
  , sepBy
  , skipSpace
  , text
  , token
  )
import Text.Syntax.Parser.Naive (Parser)

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
incommandOutput =
  \(inputLine, _, _) ->
    putStrLn $ extractParseShow parseGlobalProtocol inputLine

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
  parseFile
    filePath
    "Usage:\n\tload <relativepath>"
    (extractParseShow parseGlobalProtocol)
    id

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
    e = extractParseShow parseGlobalProtocol i

-- Show first parsed result
extractParseShow :: Show a => Parser a -> String -> String
extractParseShow p s = show $ head $ extractParse p s

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
{- pred ::= p(root,v*) = Φ inv π -}
data SymbolicPredicate =
  ESymbolicPredicate Predicate
                     [Formula]
                     FormulaDisjunct
                     Pure
  deriving (Show)

{- Φ ::= |Δ -}
type FormulaDisjunct = [Formula]

{- Δ ::= ∃v*.κ^π | Δ*Δ -}
data Formula
  = EFormulaExists [VarFirst]
                   Heap
                   Pure
  | EOpFormulaBinary Formula
                     OpFormulaBinary
                     Formula
  deriving (Show)

data OpFormulaBinary =
  EFormulaSeparate
  deriving (Show)

{- κ ::= emp | v↦d<v*> | p(v*) | κ*κ -}
data Heap
  = EHeapEmp
  | EHeapMap VarFirst
             DataStructure
             [VarFirst]
  | EHeapPredicate Predicate
                   [Formula]
  | EOpHeapBinary Heap
                  OpHeapBinary
                  Heap
  deriving (Show)

data OpHeapBinary =
  EHeapSeparate
  deriving (Show)

{- π ::= v:t | b | a | π^π | π|π | ~π | ∃v.π | ∀v.π | γ -}
data Pure
  = EPureVarType VarFirst
                 VarType
  | EPureBoole Boole
  | EPureBoolePresburger BoolePresburger
  | EPureNot Pure
  | EPureExists VarFirst
                Pure
  | EPureForall VarFirst
                Pure
  | EPurePointer Pointer
  | EOpPureBinary Pure
                  OpPureBinary
                  Pure
  deriving (Show)

data OpPureBinary
  = EPureAnd
  | EPureOr
  deriving (Show)

{- γ ::= v=v | v=null | v/=v | v/=null -}
data Pointer
  = EPointerEq VarFirst
               VarFirst
  | EPointerNull VarFirst
  | EPointerNEq VarFirst
                VarFirst
  | EPointerNNull VarFirst
  deriving (Show)

{- b ::= true | false | b=b -}
data Boole
  = EBoole Bool
  | EOpBooleBinary Boole
                   OpBooleBinary
                   Boole
  deriving (Show)

data OpBooleBinary =
  EBooleEq
  deriving (Show)

{- a ::= s=s | s<=s -}
data BoolePresburger
  = EBoolePresburgerEq Presburger
                       Presburger
  | EBoolePresburgerLeq Presburger
                        Presburger
  deriving (Show)

{- s ::= k | v | k x s | s + s | -s -}
data Presburger
  = EPresburger Integer
  | EPresburgerVarFirst VarFirst
  | EPresburgerNeg Presburger
  | EOpPresburgerBinary Presburger
                        OpPresburgerBinary
                        Presburger
  deriving (Show)

data OpPresburgerBinary
  = EPresburgerMul
  | EPresburgerAdd
  deriving (Show)

{- Figure 4.1 -}
{- G ::= S--(i)->R:c<v.Δ> | G*G | G|G | G;G | (+)(Ψ) | (-)(Ψ) | emp -}
data GlobalProtocol
  = EGlobalProtocolTransmission Role
                                Label
                                Role
                                Channel
                                VarFirst
                                Formula
  | EGlobalProtocolAssumption Assertion
  | EGlobalProtocolGuard Assertion
  | EGlobalProtocolEmp
  | EOpGlobalProtocolBinary GlobalProtocol
                            OpGlobalProtocolBinary
                            GlobalProtocol
  deriving (Show)

data OpGlobalProtocolBinary
  = EGlobalProtocolConcurrency
  | EGlobalProtocolChoice
  | EGlobalProtocolSequencing
  deriving (Show)

{- Figure 4.3 -}
{- E ::= P(i) -}
data Event =
  EEvent Role
         Label
  deriving (Show)

{- ν ::= E<CBE | E<HBE -}
data Constraint
  = EConstraintCommunicates Event
                            Event
  | EConstraintHappens Event
                       Event
  deriving (Show)

{- Ψ ::= E | ~(E) | ν | Ψ^Ψ | E==>Ψ -}
data Assertion
  = EAssertionEvent Event
  | EAssertionNEvent Event
  | EAssertionConstraint Constraint
  | EAssertionImplies Event
                      Assertion
  | EOpAssertionBinary Assertion
                       OpAssertionBinary
                       Assertion
  deriving (Show)

data OpAssertionBinary =
  EAssertionAnd
  deriving (Show)

{- Figure 4.5 -}
{- γ ::= c(i)!v.Δ | c(i)?v.Δ | γ*γ | γ|γ | γ;γ | (-)(Ψ) | (+)(Ψ) -}
data PartyProtocol
  = EPartyProtocolSend Channel
                       Label
                       VarFirst
                       Formula
  | EPartyProtocolReceive Channel
                          Label
                          VarFirst
                          Formula
  | EPartyProtocolAssumption Assertion
  | EPartyProtocolGuard Assertion
  | EPartyProtocolEmp
  | EOpPartyProtocolBinary PartyProtocol
                           OpPartyProtocolBinary
                           PartyProtocol
  deriving (Show)

data OpPartyProtocolBinary
  = EPartyProtocolConcurrency
  | EPartyProtocolChoice
  | EPartyProtocolSequencing
  deriving (Show)

{- L ::= (i)!v.Δ | (i)?v.Δ | L|L | L;L | (-)(Ψ) | (+)(Ψ) -}
data EndpointProtocol
  = EEndpointProtocolSend Channel
                          Label
                          VarFirst
                          Formula
  | EEndpointProtocolReceive Channel
                             Label
                             VarFirst
                             Formula
  -- Note:
  --    We also define L ::= L*L.
  --    See Projector.hs, SUBSECTION PER PARTY SPEC -> PER ENDPOINT SPEC Note
  --    for more details.
  | EEndpointProtocolAssumption Assertion
  | EEndpointProtocolGuard Assertion
  | EEndpointProtocolEmp
  | EOpEndpointProtocolBinary EndpointProtocol
                              OpEndpointProtocolBinary
                              EndpointProtocol
  deriving (Show)

data OpEndpointProtocolBinary
  = EEndpointProtocolConcurrency
  | EEndpointProtocolChoice
  | EEndpointProtocolSequencing
  deriving (Show)

{- Z ::= P--(i)->P:v.Δ | Z|Z | Z;Z | (-)(Ψ) | (+)(Ψ) -}
data ChannelProtocol
  = EChannelProtocolTransmission Role
                                 Label
                                 Role
                                 VarFirst
                                 Formula
  | EChannelProtocolAssumption Assertion
  | EChannelProtocolGuard Assertion
  | EChannelProtocolEmp
  | EOpChannelProtocolBinary ChannelProtocol
                             OpChannelProtocolBinary
                             ChannelProtocol
  deriving (Show)

data OpChannelProtocolBinary
  = EChannelProtocolChoice
  | EChannelProtocolSequencing
  deriving (Show)

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

angles = between (text "<") (text ">")

{-
 - SECTION SPLICE
 - TODO splice templates properly
 -}
$(defineIsomorphisms ''SymbolicPredicate)

$(defineIsomorphisms ''Formula)

$(defineIsomorphisms ''OpFormulaBinary)

$(defineIsomorphisms ''Heap)

$(defineIsomorphisms ''OpHeapBinary)

$(defineIsomorphisms ''Pure)

$(defineIsomorphisms ''OpPureBinary)

$(defineIsomorphisms ''Pointer)

$(defineIsomorphisms ''Boole)

$(defineIsomorphisms ''OpBooleBinary)

$(defineIsomorphisms ''BoolePresburger)

$(defineIsomorphisms ''Presburger)

$(defineIsomorphisms ''OpPresburgerBinary)

$(defineIsomorphisms ''GlobalProtocol)

$(defineIsomorphisms ''OpGlobalProtocolBinary)

$(defineIsomorphisms ''Event)

$(defineIsomorphisms ''Constraint)

$(defineIsomorphisms ''Assertion)

$(defineIsomorphisms ''OpAssertionBinary)

$(defineIsomorphisms ''PartyProtocol)

$(defineIsomorphisms ''OpPartyProtocolBinary)

$(defineIsomorphisms ''EndpointProtocol)

$(defineIsomorphisms ''OpEndpointProtocolBinary)

$(defineIsomorphisms ''ChannelProtocol)

$(defineIsomorphisms ''OpChannelProtocolBinary)

{-
 - SECTION PARSER
 -}
{-
 - SUBSECTION HELPERS
 -}
parseVarFirst :: Syntax delta => delta VarFirst
parseVarFirst = integer

parseDataStructure :: Syntax delta => delta DataStructure
parseDataStructure = many token

parseVarType :: Syntax delta => delta VarType
parseVarType = many token

parsePredicate :: Syntax delta => delta Predicate
parsePredicate = many token

parseRole :: Syntax delta => delta Role
parseRole = many token

parseChannel :: Syntax delta => delta Channel
parseChannel = many token

parseLabel :: Syntax delta => delta Label
parseLabel = integer

{- Figure 2.2 -}
{-
 - SUBSECTION pred
 -}
parseSymbolicPredicate :: Syntax delta => delta SymbolicPredicate
parseSymbolicPredicate = exp 0
  where
    exp 0 =
      eSymbolicPredicate <$>
      (parsePredicate <*> parens (parseFormula `sepBy` text ",") <*> text "=" *>
       parseFormulaDisjunct <*>
       text "Inv" *>
       parsePure)

{-
 - SUBSECTION Φ
 -}
parseFormulaDisjunct :: Syntax delta => delta FormulaDisjunct
parseFormulaDisjunct = exp 0
  where
    exp 0 = parseFormula `sepBy` text "|"

{-
 - SUBSECTION Δ
 -}
opFormulaBinary :: Syntax delta => delta OpFormulaBinary
opFormulaBinary = between optSpace optSpace (eFormulaSeparate <$> text "*")

prioFormulaBinary :: OpFormulaBinary -> Integer
prioFormulaBinary EFormulaSeparate = 1

parseFormula :: Syntax delta => delta Formula
parseFormula = exp 1
  where
    exp 0 =
      text "E" *> eFormulaExists <$>
      (parseVarFirst `sepBy` text "," <*> text "." *> parseHeap <*> text "^" *>
       parsePure)
    exp 1 = chainl1 (exp 0) opFormulaBinary (opPrioFormulaBinary 1)
    opPrioFormulaBinary n =
      eOpFormulaBinary . subset (\(_, (op, _)) -> prioFormulaBinary op == n)

{-
 - SUBSECTION κ
 -}
opHeapBinary :: Syntax delta => delta OpHeapBinary
opHeapBinary = between optSpace optSpace (eHeapSeparate <$> text "*")

prioHeapBinary :: OpHeapBinary -> Integer
prioHeapBinary EHeapSeparate = 1

parseHeap :: Syntax delta => delta Heap
parseHeap = exp 1
  where
    exp 0 =
      eHeapEmp <$> text "emp" <|>
      eHeapMap <$>
      (parseVarFirst <*> text "->" *> parseDataStructure <*>
       angles (parseVarFirst `sepBy` text ",")) <|>
      eHeapPredicate <$>
      (parsePredicate <*> parens (parseFormula `sepBy` text ","))
    exp 1 = chainl1 (exp 0) opHeapBinary (opPrioHeapBinary 1)
    opPrioHeapBinary n =
      eOpHeapBinary . subset (\(_, (op, _)) -> prioHeapBinary op == n)

{-
 - SUBSECTION π
 -}
opPureBinary :: Syntax delta => delta OpPureBinary
opPureBinary =
  between optSpace optSpace (ePureAnd <$> text "^" <|> ePureOr <$> text "v")

prioPureBinary :: OpPureBinary -> Integer
prioPureBinary EPureAnd = 1
prioPureBinary EPureOr = 2

parsePure :: Syntax delta => delta Pure
parsePure = exp 2
  where
    exp 0 =
      text "E" *> (ePureExists <$> (integer <*> parsePure)) <|>
      text "A" *> (ePureForall <$> (integer <*> parsePure)) <|>
      text "~" *> (ePureNot <$> parsePure) <|>
      ePureVarType <$> (parseVarFirst <*> text ":" *> parseVarType) <|>
      ePureBoole <$> parseBoole <|>
      ePurePointer <$> parsePointer <|>
      parens (skipSpace *> parsePure <* skipSpace)
    exp 1 = chainl1 (exp 0) opPureBinary (opPrioPureBinary 1)
    exp 2 = chainl1 (exp 1) opPureBinary (opPrioPureBinary 2)
    opPrioPureBinary n =
      eOpPureBinary . subset (\(_, (op, _)) -> prioPureBinary op == n)

{-
 - SUBSECTION γ
 -}
parsePointer :: Syntax delta => delta Pointer
parsePointer = exp 0
  where
    exp 0 =
      ePointerEq <$> (parseVarFirst <*> text "=" *> parseVarFirst) <|>
      ePointerNull <$> (parseVarFirst <* text "=null") <|>
      ePointerNEq <$> (parseVarFirst <*> text "/=" *> parseVarFirst) <|>
      ePointerNNull <$> (parseVarFirst <* text "/=null")

{-
 - SUBSECTION b
 -}
opBooleBinary :: Syntax delta => delta OpBooleBinary
opBooleBinary = between optSpace optSpace (eBooleEq <$> text "=")

prioBooleBinary :: OpBooleBinary -> Integer
prioBooleBinary EBooleEq = 1

parseBoole :: Syntax delta => delta Boole
parseBoole = exp 1
  where
    exp 0 = eBoole <$> bool
    exp 1 = chainl1 (exp 0) opBooleBinary (opPrioBooleBinary 1)
    opPrioBooleBinary n =
      eOpBooleBinary . subset (\(_, (op, _)) -> prioBooleBinary op == n)

{-
 - SUBSECTION a
 -}
parseBoolePresburger :: Syntax delta => delta BoolePresburger
parseBoolePresburger = exp 0
  where
    exp 0 =
      eBoolePresburgerEq <$> (parsePresburger <*> text "=" *> parsePresburger) <|>
      eBoolePresburgerLeq <$> (parsePresburger <*> text "<=" *> parsePresburger)

{-
 - SUBSECTION s
 -}
opPresburgerBinary :: Syntax delta => delta OpPresburgerBinary
opPresburgerBinary =
  between
    optSpace
    optSpace
    (ePresburgerMul <$> text "*" <|> ePresburgerAdd <$> text "+")

prioPresburgerBinary :: OpPresburgerBinary -> Integer
prioPresburgerBinary EPresburgerMul = 1
prioPresburgerBinary EPresburgerAdd = 2

parsePresburger :: Syntax delta => delta Presburger
parsePresburger = exp 2
  where
    exp 0 =
      text "-" *> (ePresburgerNeg <$> parsePresburger) <|>
      ePresburgerVarFirst <$> parseVarFirst <|>
      ePresburger <$> integer <|>
      parens (skipSpace *> parsePresburger <* skipSpace)
    exp 1 = chainl1 (exp 0) opPresburgerBinary (opPrioPresburgerBinary 1)
    exp 2 = chainl1 (exp 1) opPresburgerBinary (opPrioPresburgerBinary 2)
    opPrioPresburgerBinary n =
      eOpPresburgerBinary .
      subset (\(_, (op, _)) -> prioPresburgerBinary op == n)

{- Figure 4.1 -}
{-
 - SUBSECTION G
 -}
opGlobalProtocolBinary :: Syntax delta => delta OpGlobalProtocolBinary
opGlobalProtocolBinary =
  between
    optSpace
    optSpace
    (eGlobalProtocolConcurrency <$> text "*" <|>
     eGlobalProtocolChoice <$> text "|" <|>
     eGlobalProtocolSequencing <$> text ";")

prioGlobalProtocolBinary :: OpGlobalProtocolBinary -> Integer
prioGlobalProtocolBinary EGlobalProtocolConcurrency = 1
prioGlobalProtocolBinary EGlobalProtocolChoice = 2
prioGlobalProtocolBinary EGlobalProtocolSequencing = 3

parseGlobalProtocol :: Syntax delta => delta GlobalProtocol
parseGlobalProtocol = exp 3
  where
    exp 0 =
      eGlobalProtocolEmp <$> text "emp" <|>
      eGlobalProtocolTransmission <$>
      (parseRole <*> between (text "--") (text "->") (parens parseLabel) <*>
       parseRole <*>
       text ":" *>
       parseChannel <*>
       angles (parseVarFirst <*> text "." *> parseFormula)) <|>
      eGlobalProtocolAssumption <$> text "Assumption" *> parseAssertion <|>
      eGlobalProtocolGuard <$> text "Guard" *> parseAssertion <|>
      parens (skipSpace *> parseGlobalProtocol <* skipSpace)
    exp 1 =
      chainl1 (exp 0) opGlobalProtocolBinary (opPrioGlobalProtocolBinary 1)
    exp 2 =
      chainl1 (exp 1) opGlobalProtocolBinary (opPrioGlobalProtocolBinary 2)
    exp 3 =
      chainl1 (exp 2) opGlobalProtocolBinary (opPrioGlobalProtocolBinary 3)
    opPrioGlobalProtocolBinary n =
      eOpGlobalProtocolBinary .
      subset (\(_, (op, _)) -> prioGlobalProtocolBinary op == n)

{- Figure 4.3 -}
{-
 - SUBSECTION E
 -}
parseEvent :: Syntax delta => delta Event
parseEvent = exp 0
  where
    exp 0 = eEvent <$> (parseRole <*> parens parseLabel)

{-
 - SUBSECTION ν
 -}
parseConstraint :: Syntax delta => delta Constraint
parseConstraint = exp 0
  where
    exp 0 =
      eConstraintCommunicates <$> (parseEvent <*> text "<CB" *> parseEvent) <|>
      eConstraintHappens <$> (parseEvent <*> text "<HB" *> parseEvent)

{-
 - SUBSECTION Ψ
 -}
opAssertionBinary :: Syntax delta => delta OpAssertionBinary
opAssertionBinary = between optSpace optSpace (eAssertionAnd <$> text "^")

prioAssertionBinary :: OpAssertionBinary -> Integer
prioAssertionBinary EAssertionAnd = 1

parseAssertion :: Syntax delta => delta Assertion
parseAssertion = exp 3
  where
    exp 0 =
      eAssertionEvent <$> parseEvent <|>
      eAssertionNEvent <$> text "~" *> parens parseEvent <|>
      eAssertionConstraint <$> parseConstraint <|>
      eAssertionImplies <$> (parseEvent <*> parseAssertion) <|>
      parens (skipSpace *> parseAssertion <* skipSpace)
    exp 1 = chainl1 (exp 0) opAssertionBinary (opPrioAssertionBinary 1)
    opPrioAssertionBinary n =
      eOpAssertionBinary . subset (\(_, (op, _)) -> prioAssertionBinary op == n)

{- Figure 4.5 -}
{-
 - SUBSECTION γ
 -}
opPartyProtocolBinary :: Syntax delta => delta OpPartyProtocolBinary
opPartyProtocolBinary =
  between
    optSpace
    optSpace
    (ePartyProtocolConcurrency <$> text "*" <|>
     ePartyProtocolChoice <$> text "|" <|>
     ePartyProtocolSequencing <$> text ";")

prioPartyProtocolBinary :: OpPartyProtocolBinary -> Integer
prioPartyProtocolBinary EPartyProtocolConcurrency = 1
prioPartyProtocolBinary EPartyProtocolChoice = 2
prioPartyProtocolBinary EPartyProtocolSequencing = 3

parsePartyProtocol :: Syntax delta => delta PartyProtocol
parsePartyProtocol = exp 3
  where
    exp 0 =
      ePartyProtocolEmp <$> text "emp" <|>
      ePartyProtocolSend <$>
      (parseChannel <*> parens parseLabel <*> text "!:" *> parseVarFirst <*>
       text "." *>
       parseFormula) <|>
      ePartyProtocolReceive <$>
      (parseChannel <*> parens parseLabel <*> text "?:" *> parseVarFirst <*>
       text "." *>
       parseFormula) <|>
      ePartyProtocolAssumption <$> text "Assumption" *> parseAssertion <|>
      ePartyProtocolGuard <$> text "Guard" *> parseAssertion <|>
      parens (skipSpace *> parsePartyProtocol <* skipSpace)
    exp 1 = chainl1 (exp 0) opPartyProtocolBinary (opPrioPartyProtocolBinary 1)
    exp 2 = chainl1 (exp 1) opPartyProtocolBinary (opPrioPartyProtocolBinary 2)
    exp 3 = chainl1 (exp 2) opPartyProtocolBinary (opPrioPartyProtocolBinary 3)
    opPrioPartyProtocolBinary n =
      eOpPartyProtocolBinary .
      subset (\(_, (op, _)) -> prioPartyProtocolBinary op == n)

{-
 - SUBSECTION L
 -}
opEndpointProtocolBinary :: Syntax delta => delta OpEndpointProtocolBinary
opEndpointProtocolBinary =
  between
    optSpace
    optSpace
    (eEndpointProtocolConcurrency <$> text "*" <|>
     eEndpointProtocolChoice <$> text "|" <|>
     eEndpointProtocolSequencing <$> text ";")

prioEndpointProtocolBinary :: OpEndpointProtocolBinary -> Integer
prioEndpointProtocolBinary EEndpointProtocolConcurrency = 1
prioEndpointProtocolBinary EEndpointProtocolChoice = 2
prioEndpointProtocolBinary EEndpointProtocolSequencing = 3

parseEndpointProtocol :: Syntax delta => delta EndpointProtocol
parseEndpointProtocol = exp 3
  where
    exp 0 =
      eEndpointProtocolEmp <$> text "emp" <|>
      eEndpointProtocolSend <$>
      (parseChannel <*> parens parseLabel <*> text "!:" *> parseVarFirst <*>
       text "." *>
       parseFormula) <|>
      eEndpointProtocolReceive <$>
      (parseChannel <*> parens parseLabel <*> text "?:" *> parseVarFirst <*>
       text "." *>
       parseFormula) <|>
      eEndpointProtocolAssumption <$> text "Assumption" *> parseAssertion <|>
      eEndpointProtocolGuard <$> text "Guard" *> parseAssertion <|>
      parens (skipSpace *> parseEndpointProtocol <* skipSpace)
    exp 1 =
      chainl1 (exp 0) opEndpointProtocolBinary (opPrioEndpointProtocolBinary 1)
    exp 2 =
      chainl1 (exp 1) opEndpointProtocolBinary (opPrioEndpointProtocolBinary 2)
    exp 3 =
      chainl1 (exp 2) opEndpointProtocolBinary (opPrioEndpointProtocolBinary 3)
    opPrioEndpointProtocolBinary n =
      eOpEndpointProtocolBinary .
      subset (\(_, (op, _)) -> prioEndpointProtocolBinary op == n)

{-
 - SUBSECTION Z
 -}
opChannelProtocolBinary :: Syntax delta => delta OpChannelProtocolBinary
opChannelProtocolBinary =
  between
    optSpace
    optSpace
    (eChannelProtocolChoice <$> text "|" <|>
     eChannelProtocolSequencing <$> text ";")

prioChannelProtocolBinary :: OpChannelProtocolBinary -> Integer
prioChannelProtocolBinary EChannelProtocolChoice = 1
prioChannelProtocolBinary EChannelProtocolSequencing = 2

parseChannelProtocol :: Syntax delta => delta ChannelProtocol
parseChannelProtocol = exp 2
  where
    exp 0 =
      eChannelProtocolEmp <$> text "emp" <|>
      eChannelProtocolTransmission <$>
      (parseRole <*> between (text "--") (text "->") (parens parseLabel) <*>
       parseRole <*>
       text ":" *>
       angles (parseVarFirst <*> text "." *> parseFormula)) <|>
      eChannelProtocolAssumption <$> text "Assumption" *> parseAssertion <|>
      eChannelProtocolGuard <$> text "Guard" *> parseAssertion <|>
      parens (skipSpace *> parseChannelProtocol <* skipSpace)
    exp 1 =
      chainl1 (exp 0) opChannelProtocolBinary (opPrioChannelProtocolBinary 1)
    exp 2 =
      chainl1 (exp 1) opChannelProtocolBinary (opPrioChannelProtocolBinary 2)
    opPrioChannelProtocolBinary n =
      eOpChannelProtocolBinary .
      subset (\(_, (op, _)) -> prioChannelProtocolBinary op == n)
