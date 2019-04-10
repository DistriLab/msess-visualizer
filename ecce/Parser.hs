{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE StandaloneDeriving #-} -- Allows `deriving` on its own line
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns
{-# LANGUAGE EmptyDataDecls #-} -- Allows datatypes without constructors
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# LANGUAGE EmptyDataDeriving #-} -- Allows deriving for empty data types

-- Pragmas of invertible-parser
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
import Control.Monad (liftM)
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
  \(inputLine, _, _) -> putStrLn $ extractParseShow parseExpr inputLine

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
    (extractParseShow parseExpr)
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
    e = extractParseShow parseExpr i

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
data SymbolicPredicate
  deriving (Show)

data FormulaDisjunct
  deriving (Show)

data Formula
  deriving (Show)

data Heap
  deriving (Show)

data Pure
  deriving (Show)

data Pointer
  deriving (Show)

data BoolInteger
  deriving (Show)

{- Figure 4.1 -}
data GlobalProtocol
  deriving (Show)

{- Figure 4.3 -}
data Event
  deriving (Show)

data Constraint
  deriving (Show)

data Assertion
  deriving (Show)

{- Figure 4.5 -}
data PartyProtocol
  deriving (Show)

data EndpointProtocol
  deriving (Show)

data ChannelProtocol
  deriving (Show)

{- SUBSECTION EXPR -}
data Expr
  {- Figure 2.2 -}
  {- pred ::= p(root,v*) = Φ inv π -}
  = ESymbolicPredicate Predicate
                       [Expr]
                       FormulaDisjunct
                       Pure
  {- Φ ::= |Δ -}
  | EFormulaDisjunct [Formula]
  {- Δ ::= ∃v*.κ^π | Δ*Δ -}
  | EFormulaExists [VarFirst]
                   Heap
                   Pure
  {- κ ::= emp | v↦d<v*> | p(v*) | κ*κ -}
  | EHeapMap VarFirst
             DataStructure
             [VarFirst]
  | EHeapPredicate Predicate
                   [Expr]
  {- Figure 4.1 -}
  {- G ::= S--(i)->R:c<v.Δ> | G*G | G|G | G;G | (+)(Ψ) | (-)(Ψ) | emp -}
  | EGlobalProtocolTransmission Role
                                Label
                                Role
                                Channel
                                VarFirst
                                Formula
  {- Figure 4.3 -}
  {- E ::= P(i) -}
  | EEvent Role
           Label
  {- ν ::= E<CBE | E<HBE -}
  | EConstraintCommunicates Event
                            Event
  | EConstraintHappens Event
                       Event
  {- Ψ ::= E | ~(E) | ν | Ψ^Ψ | E==>Ψ -}
  | EAssertionEvent Event
  | EAssertionNEvent Event
  | EAssertionConstraint Constraint
  | EAssertionAnd Assertion
                  Assertion
  | EAssertionImplies Event
                      Assertion
  {- Figure 4.5 -}
  {- γ ::= c(i)!v.Δ | c(i)?v.Δ | γ*γ | γ|γ | γ;γ | (-)(Ψ) | (+)(Ψ) -}
  | EPartyProtocolSend Channel
                       Label
                       VarFirst
                       Formula
  | EPartyProtocolReceive Channel
                          Label
                          VarFirst
                          Formula
  | EPartyProtocolConcurrency PartyProtocol
                              PartyProtocol
  | EPartyProtocolChoice PartyProtocol
                         PartyProtocol
  | EPartyProtocolSequencing PartyProtocol
                             PartyProtocol
  | EPartyProtocolAssumption Assertion
  | EPartyProtocolGuard Assertion
  {- L ::= (i)!v.Δ | (i)?v.Δ | L|L | L;L | (-)(Ψ) | (+)(Ψ) -}
  | EEndpointProtocolSend Channel
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
  | EEndpointProtocolConcurrency EndpointProtocol
                                 EndpointProtocol
  | EEndpointProtocolChoice EndpointProtocol
                            EndpointProtocol
  | EEndpointProtocolSequencing EndpointProtocol
                                EndpointProtocol
  | EEndpointProtocolAssumption Assertion
  | EEndpointProtocolGuard Assertion
  {- Z ::= P--(i)->P:v.Δ | Z|Z | Z;Z | (-)(Ψ) | (+)(Ψ) -}
  | EChannelProtocolTransmission Role
                                 Label
                                 Role
                                 VarFirst
                                 Formula
  | EChannelProtocolChoice ChannelProtocol
                           ChannelProtocol
  | EChannelProtocolSequencing ChannelProtocol
                               ChannelProtocol
  | EChannelProtocolAssumption Assertion
  | EChannelProtocolGuard Assertion
  | EOpLift OpLift
  | EOpNullary OpNullary
  | EOpUnary OpUnary
             Expr
  | EOpBinary Expr
              OpBinary
              Expr

deriving instance Show Expr

{- Lifts a data type to an Expr -}
data OpLift
  {- HELPERS -}
  = EVarFirst VarFirst
  | EDataStructure DataStructure
  | EVarType VarType
  | EPredicate Predicate
  | ERole Role
  | EChannel Channel
  | ELabel Label
  {- b ::= true | false | b=b -}
  | EBool Bool
  {- s ::= k | v | k x s | s + s | -s -}
  | EInteger Integer
  deriving (Show, Eq)

data OpNullary
  {- κ ::= emp | v↦d<v*> | p(v*) | κ*κ -}
  = EHeapEmp
  {- Figure 4.1 -}
  {- G ::= S--(i)->R:c<v.Δ> | G*G | G|G | G;G | (+)(Ψ) | (-)(Ψ) | emp -}
  | EGlobalProtocolEmp
  {- γ ::= c(i)!v.Δ | c(i)?v.Δ | γ*γ | γ|γ | γ;γ | (-)(Ψ) | (+)(Ψ) -}
  | EPartyProtocolEmp
  {- L ::= (i)!v.Δ | (i)?v.Δ | L|L | L;L | (-)(Ψ) | (+)(Ψ) -}
  | EEndpointProtocolEmp
  {- Z ::= P--(i)->P:v.Δ | Z|Z | Z;Z | (-)(Ψ) | (+)(Ψ) -}
  | EChannelProtocolEmp
  deriving (Show, Eq)

data OpUnary
  {- π ::= v:t | b | a | π^π | π|π | ~π | ∃v.π | ∀v.π | γ -}
  = EPureBool
  | EPureNot
  | EPureBoolInteger
  | EPurePointer
  {- γ ::= v=v | v=null | v/=v | v/=null -}
  | EPointerNull
  | EPointerNNull
  {- s ::= k | v | k x s | s + s | -s -}
  | EIntegerNeg
  | EIntegerVarFirst
  {- Figure 4.1 -}
  {- G ::= S--(i)->R:c<v.Δ> | G*G | G|G | G;G | (+)(Ψ) | (-)(Ψ) | emp -}
  | EGlobalProtocolAssumption
  | EGlobalProtocolGuard
  deriving (Show, Eq)

data OpBinary
  {- κ ::= emp | v↦d<v*> | p(v*) | κ*κ -}
  = EHeapSeparate
  {- Δ ::= ∃v*.κ^π | Δ*Δ -}
  | EFormulaSeparate
  | EPureAnd
  | EPureOr
  {- π ::= v:t | b | a | π^π | π|π | ~π | ∃v.π | ∀v.π | γ -}
  | EPureVarType
  | EPureExists
  | EPureForall
  {- γ ::= v=v | v=null | v/=v | v/=null -}
  | EPointerEq
  | EPointerNEq
  {- b ::= true | false | b=b -}
  | EBoolEq
  {- a ::= s=s | s<=s -}
  | EBoolIntegerEq
  | EBoolIntegerLeq
  {- s ::= k | v | k x s | s + s | -s -}
  | EIntegerMul
  | EIntegerAdd
  {- Figure 4.1 -}
  {- G ::= S--(i)->R:c<v.Δ> | G*G | G|G | G;G | (+)(Ψ) | (-)(Ψ) | emp -}
  | EGlobalProtocolConcurrency
  | EGlobalProtocolChoice
  | EGlobalProtocolSequencing
  deriving (Show, Eq)

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

{-
 - SECTION SPLICE
 - TODO splice templates properly
 -}
-- $(defineIsomorphisms ''Expr)
eSymbolicPredicate :: Iso (Predicate, ([Expr], (FormulaDisjunct, Pure))) Expr
eSymbolicPredicate =
  (Iso
     (\(x_ae6a, (x_ae6b, (x_ae6c, x_ae6d))) ->
        Just ((((ESymbolicPredicate x_ae6a) x_ae6b) x_ae6c) x_ae6d)))
    (\x_ae6e ->
       case x_ae6e of
         ESymbolicPredicate x_ae6a x_ae6b x_ae6c x_ae6d ->
           Just (x_ae6a, (x_ae6b, (x_ae6c, x_ae6d)))
         _ -> Nothing)

eFormulaDisjunct :: Iso [Formula] Expr
eFormulaDisjunct =
  (Iso (\x_ae6f -> Just (EFormulaDisjunct x_ae6f)))
    (\x_ae6g ->
       case x_ae6g of
         EFormulaDisjunct x_ae6f -> Just x_ae6f
         _ -> Nothing)

eFormulaExists :: Iso ([VarFirst], (Heap, Pure)) Expr
eFormulaExists =
  (Iso
     (\(x_ae6h, (x_ae6i, x_ae6j)) ->
        Just (((EFormulaExists x_ae6h) x_ae6i) x_ae6j)))
    (\x_ae6k ->
       case x_ae6k of
         EFormulaExists x_ae6h x_ae6i x_ae6j -> Just (x_ae6h, (x_ae6i, x_ae6j))
         _ -> Nothing)

eHeapMap :: Iso (VarFirst, (DataStructure, [VarFirst])) Expr
eHeapMap =
  (Iso (\(x_ae6l, (x_ae6m, x_ae6n)) -> Just (((EHeapMap x_ae6l) x_ae6m) x_ae6n)))
    (\x_ae6o ->
       case x_ae6o of
         EHeapMap x_ae6l x_ae6m x_ae6n -> Just (x_ae6l, (x_ae6m, x_ae6n))
         _ -> Nothing)

eHeapPredicate :: Iso (Predicate, [Expr]) Expr
eHeapPredicate =
  (Iso (\(x_ae6p, x_ae6q) -> Just ((EHeapPredicate x_ae6p) x_ae6q)))
    (\x_ae6r ->
       case x_ae6r of
         EHeapPredicate x_ae6p x_ae6q -> Just (x_ae6p, x_ae6q)
         _ -> Nothing)

eGlobalProtocolTransmission ::
     Iso (Role, (Label, (Role, (Channel, (VarFirst, Formula))))) Expr
eGlobalProtocolTransmission =
  (Iso
     (\(x_ae6s, (x_ae6t, (x_ae6u, (x_ae6v, (x_ae6w, x_ae6x))))) ->
        Just
          ((((((EGlobalProtocolTransmission x_ae6s) x_ae6t) x_ae6u) x_ae6v)
              x_ae6w)
             x_ae6x)))
    (\x_ae6y ->
       case x_ae6y of
         EGlobalProtocolTransmission x_ae6s x_ae6t x_ae6u x_ae6v x_ae6w x_ae6x ->
           Just (x_ae6s, (x_ae6t, (x_ae6u, (x_ae6v, (x_ae6w, x_ae6x)))))
         _ -> Nothing)

eEvent :: Iso (Role, Label) Expr
eEvent =
  (Iso (\(x_ae6z, x_ae6A) -> Just ((EEvent x_ae6z) x_ae6A)))
    (\x_ae6B ->
       case x_ae6B of
         EEvent x_ae6z x_ae6A -> Just (x_ae6z, x_ae6A)
         _ -> Nothing)

eConstraintCommunicates :: Iso (Event, Event) Expr
eConstraintCommunicates =
  (Iso (\(x_ae6C, x_ae6D) -> Just ((EConstraintCommunicates x_ae6C) x_ae6D)))
    (\x_ae6E ->
       case x_ae6E of
         EConstraintCommunicates x_ae6C x_ae6D -> Just (x_ae6C, x_ae6D)
         _ -> Nothing)

eConstraintHappens :: Iso (Event, Event) Expr
eConstraintHappens =
  (Iso (\(x_ae6F, x_ae6G) -> Just ((EConstraintHappens x_ae6F) x_ae6G)))
    (\x_ae6H ->
       case x_ae6H of
         EConstraintHappens x_ae6F x_ae6G -> Just (x_ae6F, x_ae6G)
         _ -> Nothing)

eAssertionEvent :: Iso Event Expr
eAssertionEvent =
  (Iso (\x_ae6I -> Just (EAssertionEvent x_ae6I)))
    (\x_ae6J ->
       case x_ae6J of
         EAssertionEvent x_ae6I -> Just x_ae6I
         _ -> Nothing)

eAssertionNEvent :: Iso Event Expr
eAssertionNEvent =
  (Iso (\x_ae6K -> Just (EAssertionNEvent x_ae6K)))
    (\x_ae6L ->
       case x_ae6L of
         EAssertionNEvent x_ae6K -> Just x_ae6K
         _ -> Nothing)

eAssertionConstraint :: Iso Constraint Expr
eAssertionConstraint =
  (Iso (\x_ae6M -> Just (EAssertionConstraint x_ae6M)))
    (\x_ae6N ->
       case x_ae6N of
         EAssertionConstraint x_ae6M -> Just x_ae6M
         _ -> Nothing)

eAssertionAnd :: Iso (Assertion, Assertion) Expr
eAssertionAnd =
  (Iso (\(x_ae6O, x_ae6P) -> Just ((EAssertionAnd x_ae6O) x_ae6P)))
    (\x_ae6Q ->
       case x_ae6Q of
         EAssertionAnd x_ae6O x_ae6P -> Just (x_ae6O, x_ae6P)
         _ -> Nothing)

eAssertionImplies :: Iso (Event, Assertion) Expr
eAssertionImplies =
  (Iso (\(x_ae6R, x_ae6S) -> Just ((EAssertionImplies x_ae6R) x_ae6S)))
    (\x_ae6T ->
       case x_ae6T of
         EAssertionImplies x_ae6R x_ae6S -> Just (x_ae6R, x_ae6S)
         _ -> Nothing)

ePartyProtocolSend :: Iso (Channel, (Label, (VarFirst, Formula))) Expr
ePartyProtocolSend =
  (Iso
     (\(x_ae6U, (x_ae6V, (x_ae6W, x_ae6X))) ->
        Just ((((EPartyProtocolSend x_ae6U) x_ae6V) x_ae6W) x_ae6X)))
    (\x_ae6Y ->
       case x_ae6Y of
         EPartyProtocolSend x_ae6U x_ae6V x_ae6W x_ae6X ->
           Just (x_ae6U, (x_ae6V, (x_ae6W, x_ae6X)))
         _ -> Nothing)

ePartyProtocolReceive :: Iso (Channel, (Label, (VarFirst, Formula))) Expr
ePartyProtocolReceive =
  (Iso
     (\(x_ae6Z, (x_ae70, (x_ae71, x_ae72))) ->
        Just ((((EPartyProtocolReceive x_ae6Z) x_ae70) x_ae71) x_ae72)))
    (\x_ae73 ->
       case x_ae73 of
         EPartyProtocolReceive x_ae6Z x_ae70 x_ae71 x_ae72 ->
           Just (x_ae6Z, (x_ae70, (x_ae71, x_ae72)))
         _ -> Nothing)

ePartyProtocolConcurrency :: Iso (PartyProtocol, PartyProtocol) Expr
ePartyProtocolConcurrency =
  (Iso (\(x_ae74, x_ae75) -> Just ((EPartyProtocolConcurrency x_ae74) x_ae75)))
    (\x_ae76 ->
       case x_ae76 of
         EPartyProtocolConcurrency x_ae74 x_ae75 -> Just (x_ae74, x_ae75)
         _ -> Nothing)

ePartyProtocolChoice :: Iso (PartyProtocol, PartyProtocol) Expr
ePartyProtocolChoice =
  (Iso (\(x_ae77, x_ae78) -> Just ((EPartyProtocolChoice x_ae77) x_ae78)))
    (\x_ae79 ->
       case x_ae79 of
         EPartyProtocolChoice x_ae77 x_ae78 -> Just (x_ae77, x_ae78)
         _ -> Nothing)

ePartyProtocolSequencing :: Iso (PartyProtocol, PartyProtocol) Expr
ePartyProtocolSequencing =
  (Iso (\(x_ae7a, x_ae7b) -> Just ((EPartyProtocolSequencing x_ae7a) x_ae7b)))
    (\x_ae7c ->
       case x_ae7c of
         EPartyProtocolSequencing x_ae7a x_ae7b -> Just (x_ae7a, x_ae7b)
         _ -> Nothing)

ePartyProtocolAssumption :: Iso Assertion Expr
ePartyProtocolAssumption =
  (Iso (\x_ae7d -> Just (EPartyProtocolAssumption x_ae7d)))
    (\x_ae7e ->
       case x_ae7e of
         EPartyProtocolAssumption x_ae7d -> Just x_ae7d
         _ -> Nothing)

ePartyProtocolGuard :: Iso Assertion Expr
ePartyProtocolGuard =
  (Iso (\x_ae7f -> Just (EPartyProtocolGuard x_ae7f)))
    (\x_ae7g ->
       case x_ae7g of
         EPartyProtocolGuard x_ae7f -> Just x_ae7f
         _ -> Nothing)

eEndpointProtocolSend :: Iso (Channel, (Label, (VarFirst, Formula))) Expr
eEndpointProtocolSend =
  (Iso
     (\(x_ae7h, (x_ae7i, (x_ae7j, x_ae7k))) ->
        Just ((((EEndpointProtocolSend x_ae7h) x_ae7i) x_ae7j) x_ae7k)))
    (\x_ae7l ->
       case x_ae7l of
         EEndpointProtocolSend x_ae7h x_ae7i x_ae7j x_ae7k ->
           Just (x_ae7h, (x_ae7i, (x_ae7j, x_ae7k)))
         _ -> Nothing)

eEndpointProtocolReceive :: Iso (Channel, (Label, (VarFirst, Formula))) Expr
eEndpointProtocolReceive =
  (Iso
     (\(x_ae7m, (x_ae7n, (x_ae7o, x_ae7p))) ->
        Just ((((EEndpointProtocolReceive x_ae7m) x_ae7n) x_ae7o) x_ae7p)))
    (\x_ae7q ->
       case x_ae7q of
         EEndpointProtocolReceive x_ae7m x_ae7n x_ae7o x_ae7p ->
           Just (x_ae7m, (x_ae7n, (x_ae7o, x_ae7p)))
         _ -> Nothing)

eEndpointProtocolConcurrency :: Iso (EndpointProtocol, EndpointProtocol) Expr
eEndpointProtocolConcurrency =
  (Iso
     (\(x_ae7r, x_ae7s) -> Just ((EEndpointProtocolConcurrency x_ae7r) x_ae7s)))
    (\x_ae7t ->
       case x_ae7t of
         EEndpointProtocolConcurrency x_ae7r x_ae7s -> Just (x_ae7r, x_ae7s)
         _ -> Nothing)

eEndpointProtocolChoice :: Iso (EndpointProtocol, EndpointProtocol) Expr
eEndpointProtocolChoice =
  (Iso (\(x_ae7u, x_ae7v) -> Just ((EEndpointProtocolChoice x_ae7u) x_ae7v)))
    (\x_ae7w ->
       case x_ae7w of
         EEndpointProtocolChoice x_ae7u x_ae7v -> Just (x_ae7u, x_ae7v)
         _ -> Nothing)

eEndpointProtocolSequencing :: Iso (EndpointProtocol, EndpointProtocol) Expr
eEndpointProtocolSequencing =
  (Iso (\(x_ae7x, x_ae7y) -> Just ((EEndpointProtocolSequencing x_ae7x) x_ae7y)))
    (\x_ae7z ->
       case x_ae7z of
         EEndpointProtocolSequencing x_ae7x x_ae7y -> Just (x_ae7x, x_ae7y)
         _ -> Nothing)

eEndpointProtocolAssumption :: Iso Assertion Expr
eEndpointProtocolAssumption =
  (Iso (\x_ae7A -> Just (EEndpointProtocolAssumption x_ae7A)))
    (\x_ae7B ->
       case x_ae7B of
         EEndpointProtocolAssumption x_ae7A -> Just x_ae7A
         _ -> Nothing)

eEndpointProtocolGuard :: Iso Assertion Expr
eEndpointProtocolGuard =
  (Iso (\x_ae7C -> Just (EEndpointProtocolGuard x_ae7C)))
    (\x_ae7D ->
       case x_ae7D of
         EEndpointProtocolGuard x_ae7C -> Just x_ae7C
         _ -> Nothing)

eChannelProtocolTransmission ::
     Iso (Role, (Label, (Role, (VarFirst, Formula)))) Expr
eChannelProtocolTransmission =
  (Iso
     (\(x_ae7E, (x_ae7F, (x_ae7G, (x_ae7H, x_ae7I)))) ->
        Just
          (((((EChannelProtocolTransmission x_ae7E) x_ae7F) x_ae7G) x_ae7H)
             x_ae7I)))
    (\x_ae7J ->
       case x_ae7J of
         EChannelProtocolTransmission x_ae7E x_ae7F x_ae7G x_ae7H x_ae7I ->
           Just (x_ae7E, (x_ae7F, (x_ae7G, (x_ae7H, x_ae7I))))
         _ -> Nothing)

eChannelProtocolChoice :: Iso (ChannelProtocol, ChannelProtocol) Expr
eChannelProtocolChoice =
  (Iso (\(x_ae7K, x_ae7L) -> Just ((EChannelProtocolChoice x_ae7K) x_ae7L)))
    (\x_ae7M ->
       case x_ae7M of
         EChannelProtocolChoice x_ae7K x_ae7L -> Just (x_ae7K, x_ae7L)
         _ -> Nothing)

eChannelProtocolSequencing :: Iso (ChannelProtocol, ChannelProtocol) Expr
eChannelProtocolSequencing =
  (Iso (\(x_ae7N, x_ae7O) -> Just ((EChannelProtocolSequencing x_ae7N) x_ae7O)))
    (\x_ae7P ->
       case x_ae7P of
         EChannelProtocolSequencing x_ae7N x_ae7O -> Just (x_ae7N, x_ae7O)
         _ -> Nothing)

eChannelProtocolAssumption :: Iso Assertion Expr
eChannelProtocolAssumption =
  (Iso (\x_ae7Q -> Just (EChannelProtocolAssumption x_ae7Q)))
    (\x_ae7R ->
       case x_ae7R of
         EChannelProtocolAssumption x_ae7Q -> Just x_ae7Q
         _ -> Nothing)

eChannelProtocolGuard :: Iso Assertion Expr
eChannelProtocolGuard =
  (Iso (\x_ae7S -> Just (EChannelProtocolGuard x_ae7S)))
    (\x_ae7T ->
       case x_ae7T of
         EChannelProtocolGuard x_ae7S -> Just x_ae7S
         _ -> Nothing)

eOpLift :: Iso OpLift Expr
eOpLift =
  (Iso (\x_ae7U -> Just (EOpLift x_ae7U)))
    (\x_ae7V ->
       case x_ae7V of
         EOpLift x_ae7U -> Just x_ae7U
         _ -> Nothing)

eOpNullary :: Iso OpNullary Expr
eOpNullary =
  (Iso (\x_ae7W -> Just (EOpNullary x_ae7W)))
    (\x_ae7X ->
       case x_ae7X of
         EOpNullary x_ae7W -> Just x_ae7W
         _ -> Nothing)

eOpUnary :: Iso (OpUnary, Expr) Expr
eOpUnary =
  (Iso (\(x_ae7Y, x_ae7Z) -> Just ((EOpUnary x_ae7Y) x_ae7Z)))
    (\x_ae80 ->
       case x_ae80 of
         EOpUnary x_ae7Y x_ae7Z -> Just (x_ae7Y, x_ae7Z)
         _ -> Nothing)

eOpBinary :: Iso (Expr, (OpBinary, Expr)) Expr
eOpBinary =
  (Iso
     (\(x_ae81, (x_ae82, x_ae83)) -> Just (((EOpBinary x_ae81) x_ae82) x_ae83)))
    (\x_ae84 ->
       case x_ae84 of
         EOpBinary x_ae81 x_ae82 x_ae83 -> Just (x_ae81, (x_ae82, x_ae83))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpLift)
eVarFirst :: Iso VarFirst OpLift
eVarFirst =
  (Iso (\x_aek3 -> Just (EVarFirst x_aek3)))
    (\x_aek4 ->
       case x_aek4 of
         EVarFirst x_aek3 -> Just x_aek3
         _ -> Nothing)

eDataStructure :: Iso DataStructure OpLift
eDataStructure =
  (Iso (\x_aek5 -> Just (EDataStructure x_aek5)))
    (\x_aek6 ->
       case x_aek6 of
         EDataStructure x_aek5 -> Just x_aek5
         _ -> Nothing)

eVarType :: Iso VarType OpLift
eVarType =
  (Iso (\x_aek7 -> Just (EVarType x_aek7)))
    (\x_aek8 ->
       case x_aek8 of
         EVarType x_aek7 -> Just x_aek7
         _ -> Nothing)

ePredicate :: Iso Predicate OpLift
ePredicate =
  (Iso (\x_aek9 -> Just (EPredicate x_aek9)))
    (\x_aeka ->
       case x_aeka of
         EPredicate x_aek9 -> Just x_aek9
         _ -> Nothing)

eRole :: Iso Role OpLift
eRole =
  (Iso (\x_aekb -> Just (ERole x_aekb)))
    (\x_aekc ->
       case x_aekc of
         ERole x_aekb -> Just x_aekb
         _ -> Nothing)

eChannel :: Iso Channel OpLift
eChannel =
  (Iso (\x_aekd -> Just (EChannel x_aekd)))
    (\x_aeke ->
       case x_aeke of
         EChannel x_aekd -> Just x_aekd
         _ -> Nothing)

eLabel :: Iso Label OpLift
eLabel =
  (Iso (\x_aekf -> Just (ELabel x_aekf)))
    (\x_aekg ->
       case x_aekg of
         ELabel x_aekf -> Just x_aekf
         _ -> Nothing)

eBool :: Iso Bool OpLift
eBool =
  (Iso (\x_aekh -> Just (EBool x_aekh)))
    (\x_aeki ->
       case x_aeki of
         EBool x_aekh -> Just x_aekh
         _ -> Nothing)

eInteger :: Iso Integer OpLift
eInteger =
  (Iso (\x_aekj -> Just (EInteger x_aekj)))
    (\x_aekk ->
       case x_aekk of
         EInteger x_aekj -> Just x_aekj
         _ -> Nothing)

-- $(defineIsomorphisms ''OpNullary)
eHeapEmp :: Iso () OpNullary
eHeapEmp =
  (Iso (\() -> Just EHeapEmp))
    (\x_aemD ->
       case x_aemD of
         EHeapEmp -> Just ()
         _ -> Nothing)

eGlobalProtocolEmp :: Iso () OpNullary
eGlobalProtocolEmp =
  (Iso (\() -> Just EGlobalProtocolEmp))
    (\x_aemE ->
       case x_aemE of
         EGlobalProtocolEmp -> Just ()
         _ -> Nothing)

ePartyProtocolEmp :: Iso () OpNullary
ePartyProtocolEmp =
  (Iso (\() -> Just EPartyProtocolEmp))
    (\x_aemF ->
       case x_aemF of
         EPartyProtocolEmp -> Just ()
         _ -> Nothing)

eEndpointProtocolEmp :: Iso () OpNullary
eEndpointProtocolEmp =
  (Iso (\() -> Just EEndpointProtocolEmp))
    (\x_aemG ->
       case x_aemG of
         EEndpointProtocolEmp -> Just ()
         _ -> Nothing)

eChannelProtocolEmp :: Iso () OpNullary
eChannelProtocolEmp =
  (Iso (\() -> Just EChannelProtocolEmp))
    (\x_aemH ->
       case x_aemH of
         EChannelProtocolEmp -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''OpUnary)
ePureBool :: Iso () OpUnary
ePureBool =
  (Iso (\() -> Just EPureBool))
    (\x_aeo2 ->
       case x_aeo2 of
         EPureBool -> Just ()
         _ -> Nothing)

ePureNot :: Iso () OpUnary
ePureNot =
  (Iso (\() -> Just EPureNot))
    (\x_aeo3 ->
       case x_aeo3 of
         EPureNot -> Just ()
         _ -> Nothing)

ePureBoolInteger :: Iso () OpUnary
ePureBoolInteger =
  (Iso (\() -> Just EPureBoolInteger))
    (\x_aeo4 ->
       case x_aeo4 of
         EPureBoolInteger -> Just ()
         _ -> Nothing)

ePurePointer :: Iso () OpUnary
ePurePointer =
  (Iso (\() -> Just EPurePointer))
    (\x_aeo5 ->
       case x_aeo5 of
         EPurePointer -> Just ()
         _ -> Nothing)

ePointerNull :: Iso () OpUnary
ePointerNull =
  (Iso (\() -> Just EPointerNull))
    (\x_aeo6 ->
       case x_aeo6 of
         EPointerNull -> Just ()
         _ -> Nothing)

ePointerNNull :: Iso () OpUnary
ePointerNNull =
  (Iso (\() -> Just EPointerNNull))
    (\x_aeo7 ->
       case x_aeo7 of
         EPointerNNull -> Just ()
         _ -> Nothing)

eIntegerNeg :: Iso () OpUnary
eIntegerNeg =
  (Iso (\() -> Just EIntegerNeg))
    (\x_aeo8 ->
       case x_aeo8 of
         EIntegerNeg -> Just ()
         _ -> Nothing)

eIntegerVarFirst :: Iso () OpUnary
eIntegerVarFirst =
  (Iso (\() -> Just EIntegerVarFirst))
    (\x_aeo9 ->
       case x_aeo9 of
         EIntegerVarFirst -> Just ()
         _ -> Nothing)

eGlobalProtocolAssumption :: Iso () OpUnary
eGlobalProtocolAssumption =
  (Iso (\() -> Just EGlobalProtocolAssumption))
    (\x_aeoa ->
       case x_aeoa of
         EGlobalProtocolAssumption -> Just ()
         _ -> Nothing)

eGlobalProtocolGuard :: Iso () OpUnary
eGlobalProtocolGuard =
  (Iso (\() -> Just EGlobalProtocolGuard))
    (\x_aeob ->
       case x_aeob of
         EGlobalProtocolGuard -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''OpBinary)
eHeapSeparate :: Iso () OpBinary
eHeapSeparate =
  (Iso (\() -> Just EHeapSeparate))
    (\x_aeqJ ->
       case x_aeqJ of
         EHeapSeparate -> Just ()
         _ -> Nothing)

eFormulaSeparate :: Iso () OpBinary
eFormulaSeparate =
  (Iso (\() -> Just EFormulaSeparate))
    (\x_aeqK ->
       case x_aeqK of
         EFormulaSeparate -> Just ()
         _ -> Nothing)

ePureAnd :: Iso () OpBinary
ePureAnd =
  (Iso (\() -> Just EPureAnd))
    (\x_aeqL ->
       case x_aeqL of
         EPureAnd -> Just ()
         _ -> Nothing)

ePureOr :: Iso () OpBinary
ePureOr =
  (Iso (\() -> Just EPureOr))
    (\x_aeqM ->
       case x_aeqM of
         EPureOr -> Just ()
         _ -> Nothing)

ePureVarType :: Iso () OpBinary
ePureVarType =
  (Iso (\() -> Just EPureVarType))
    (\x_aeqN ->
       case x_aeqN of
         EPureVarType -> Just ()
         _ -> Nothing)

ePureExists :: Iso () OpBinary
ePureExists =
  (Iso (\() -> Just EPureExists))
    (\x_aeqO ->
       case x_aeqO of
         EPureExists -> Just ()
         _ -> Nothing)

ePureForall :: Iso () OpBinary
ePureForall =
  (Iso (\() -> Just EPureForall))
    (\x_aeqP ->
       case x_aeqP of
         EPureForall -> Just ()
         _ -> Nothing)

ePointerEq :: Iso () OpBinary
ePointerEq =
  (Iso (\() -> Just EPointerEq))
    (\x_aeqQ ->
       case x_aeqQ of
         EPointerEq -> Just ()
         _ -> Nothing)

ePointerNEq :: Iso () OpBinary
ePointerNEq =
  (Iso (\() -> Just EPointerNEq))
    (\x_aeqR ->
       case x_aeqR of
         EPointerNEq -> Just ()
         _ -> Nothing)

eBoolEq :: Iso () OpBinary
eBoolEq =
  (Iso (\() -> Just EBoolEq))
    (\x_aeqS ->
       case x_aeqS of
         EBoolEq -> Just ()
         _ -> Nothing)

eBoolIntegerEq :: Iso () OpBinary
eBoolIntegerEq =
  (Iso (\() -> Just EBoolIntegerEq))
    (\x_aeqT ->
       case x_aeqT of
         EBoolIntegerEq -> Just ()
         _ -> Nothing)

eBoolIntegerLeq :: Iso () OpBinary
eBoolIntegerLeq =
  (Iso (\() -> Just EBoolIntegerLeq))
    (\x_aeqU ->
       case x_aeqU of
         EBoolIntegerLeq -> Just ()
         _ -> Nothing)

eIntegerMul :: Iso () OpBinary
eIntegerMul =
  (Iso (\() -> Just EIntegerMul))
    (\x_aeqV ->
       case x_aeqV of
         EIntegerMul -> Just ()
         _ -> Nothing)

eIntegerAdd :: Iso () OpBinary
eIntegerAdd =
  (Iso (\() -> Just EIntegerAdd))
    (\x_aeqW ->
       case x_aeqW of
         EIntegerAdd -> Just ()
         _ -> Nothing)

eGlobalProtocolConcurrency :: Iso () OpBinary
eGlobalProtocolConcurrency =
  (Iso (\() -> Just EGlobalProtocolConcurrency))
    (\x_aeqX ->
       case x_aeqX of
         EGlobalProtocolConcurrency -> Just ()
         _ -> Nothing)

eGlobalProtocolChoice :: Iso () OpBinary
eGlobalProtocolChoice =
  (Iso (\() -> Just EGlobalProtocolChoice))
    (\x_aeqY ->
       case x_aeqY of
         EGlobalProtocolChoice -> Just ()
         _ -> Nothing)

eGlobalProtocolSequencing :: Iso () OpBinary
eGlobalProtocolSequencing =
  (Iso (\() -> Just EGlobalProtocolSequencing))
    (\x_aeqZ ->
       case x_aeqZ of
         EGlobalProtocolSequencing -> Just ()
         _ -> Nothing)

{-
 - SECTION PARSER
 -}
{-
 - SUBSECTION HELPERS
 -}
parseExpr :: Syntax delta => delta Expr
parseExpr = parsePure

opPureUnary :: Syntax delta => delta OpUnary
opPureUnary = ePureNot <$> text "~"

opPureBinary :: Syntax delta => delta OpBinary
opPureBinary = ePureAnd <$> text "^" <|> ePureOr <$> text "v"

spacedOpPureBinary :: Syntax delta => delta OpBinary
spacedOpPureBinary = between optSpace optSpace opPureBinary

prioPureBinary :: OpBinary -> Integer
prioPureBinary EPureAnd = 1
prioPureBinary EPureOr = 2

opBoolBinary :: Syntax delta => delta OpBinary
opBoolBinary = eBoolEq <$> text "="

prioBoolBinary :: OpBinary -> Integer
prioBoolBinary EBoolEq = 1

spacedOpBoolBinary :: Syntax delta => delta OpBinary
spacedOpBoolBinary = between optSpace optSpace opBoolBinary

opIntegerUnary :: Syntax delta => delta OpUnary
opIntegerUnary = eIntegerNeg <$> text "-"

opIntegerBinary :: Syntax delta => delta OpBinary
opIntegerBinary = eIntegerMul <$> text "*" <|> eIntegerAdd <$> text "+"

spacedOpIntegerBinary :: Syntax delta => delta OpBinary
spacedOpIntegerBinary = between optSpace optSpace opIntegerBinary

prioIntegerBinary :: OpBinary -> Integer
prioIntegerBinary EIntegerMul = 1
prioIntegerBinary EIntegerAdd = 2

parseVarFirst = liftM EVarFirst integer

parseDataStructure = liftM EDataStructure identifier

parseVarType = liftM EVarType identifier

parsePredicate = liftM EPredicate identifier

parseRole = liftM ERole identifier

parseChannel = liftM EChannel identifier

parseLabel = liftM ELabel integer

parsePure :: Syntax delta => delta Expr
parsePure = exp 2
  where
    exp 0 =
      eOpUnary <$> (opPureUnary <*> parsePure) <|>
      eOpUnary <$> ((ePureBool <$> text "") <*> parseBool) <|>
      parens (skipSpace *> parsePure <* skipSpace)
    exp 1 = chainl1 (exp 0) spacedOpPureBinary (opPrioPureBinary 1)
    exp 2 = chainl1 (exp 1) spacedOpPureBinary (opPrioPureBinary 2)
    opPrioPureBinary n =
      eOpBinary . subset (\(_, (op, _)) -> prioPureBinary op == n)

parseBool :: Syntax delta => delta Expr
parseBool = exp 1
  where
    exp 0 = eOpLift <$> (eBool <$> bool)
    exp 1 = chainl1 (exp 0) spacedOpBoolBinary (opPrioBoolBinary 1)
    opPrioBoolBinary n =
      eOpBinary . subset (\(_, (op, _)) -> prioBoolBinary op == n)

parseInteger :: Syntax delta => delta Expr
parseInteger = exp 2
  where
    exp 0 =
      eOpUnary <$> (opIntegerUnary <*> (eOpLift <$> (eInteger <$> integer))) <|>
      eOpLift <$> (eInteger <$> integer) <|>
      parens (skipSpace *> parseInteger <* skipSpace)
    exp 1 = chainl1 (exp 0) spacedOpIntegerBinary (opPrioIntegerBinary 1)
    exp 2 = chainl1 (exp 1) spacedOpIntegerBinary (opPrioIntegerBinary 2)
    opPrioIntegerBinary n =
      eOpBinary . subset (\(_, (op, _)) -> prioIntegerBinary op == n)
{- Figure 2.2 -}
{-
 - SUBSECTION pred
 -}
{-
 - SUBSECTION Φ
 -}
-- Disjunct must have at least 2 formulas
{-
 - SUBSECTION Δ
 -}
{-
 - SUBSECTION κ
 -}
{-
 - SUBSECTION π
 -}
{-
 - SUBSECTION γ
 -}
{-
 - SUBSECTION b
 -}
{-
 - SUBSECTION a
 -}
{-
 - SUBSECTION s
 -}
{- Figure 4.1 -}
{-
 - SUBSECTION G
 -}
{- Figure 4.3 -}
{-
 - SUBSECTION E
 -}
{-
 - SUBSECTION ν
 -}
{-
 - SUBSECTION Ψ
 -}
{- Figure 4.5 -}
{-
 - SUBSECTION γ
 -}
{-
 - SUBSECTION L
 -}
{-
 - SUBSECTION Z
 -}
