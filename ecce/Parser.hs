{-
 - SECTION PRAGMAS
 -}
-- Pragmas of invertible-parser
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-} -- Allows `deriving` on its own line
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns
{-# LANGUAGE EmptyDataDecls #-} -- Allows datatypes without constructors
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
  {- HELPERS -}
  = EVarFirst VarFirst
  | EDataStructure DataStructure
  | EVarType VarType
  | EPredicate Predicate
  | ERole Role
  | EChannel Channel
  | ELabel Label
  {- Figure 2.2 -}
  {- pred ::= p(root,v*) = Φ inv π -}
  | ESymbolicPredicate Predicate
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
  | EHeapEmp
  | EHeapMap VarFirst
             DataStructure
             [VarFirst]
  | EHeapPredicate Predicate
                   [Expr]
  {- b ::= true | false | b=b -}
  | EBool Bool
  {- s ::= k | v | k x s | s + s | -s -}
  | EInteger Integer
  {- Figure 4.1 -}
  {- G ::= S--(i)->R:c<v.Δ> | G*G | G|G | G;G | (+)(Ψ) | (-)(Ψ) | emp -}
  | EGlobalProtocolTransmission Role
                                Label
                                Role
                                Channel
                                VarFirst
                                Formula
  | EGlobalProtocolEmp
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
  | EPartyProtocolEmp
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
  | EEndpointProtocolEmp
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
  | EChannelProtocolEmp
  | EOpUnary OpUnary
             Expr
  | EOpBinary Expr
              OpBinary
              Expr

deriving instance Show Expr

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
eVarFirst :: Iso VarFirst Expr
eVarFirst =
  (Iso (\x_ae24 -> Just (EVarFirst x_ae24)))
    (\x_ae25 ->
       case x_ae25 of
         EVarFirst x_ae24 -> Just x_ae24
         _ -> Nothing)

eDataStructure :: Iso DataStructure Expr
eDataStructure =
  (Iso (\x_ae26 -> Just (EDataStructure x_ae26)))
    (\x_ae27 ->
       case x_ae27 of
         EDataStructure x_ae26 -> Just x_ae26
         _ -> Nothing)

eVarType :: Iso VarType Expr
eVarType =
  (Iso (\x_ae28 -> Just (EVarType x_ae28)))
    (\x_ae29 ->
       case x_ae29 of
         EVarType x_ae28 -> Just x_ae28
         _ -> Nothing)

ePredicate :: Iso Predicate Expr
ePredicate =
  (Iso (\x_ae2a -> Just (EPredicate x_ae2a)))
    (\x_ae2b ->
       case x_ae2b of
         EPredicate x_ae2a -> Just x_ae2a
         _ -> Nothing)

eRole :: Iso Role Expr
eRole =
  (Iso (\x_ae2c -> Just (ERole x_ae2c)))
    (\x_ae2d ->
       case x_ae2d of
         ERole x_ae2c -> Just x_ae2c
         _ -> Nothing)

eChannel :: Iso Channel Expr
eChannel =
  (Iso (\x_ae2e -> Just (EChannel x_ae2e)))
    (\x_ae2f ->
       case x_ae2f of
         EChannel x_ae2e -> Just x_ae2e
         _ -> Nothing)

eLabel :: Iso Label Expr
eLabel =
  (Iso (\x_ae2g -> Just (ELabel x_ae2g)))
    (\x_ae2h ->
       case x_ae2h of
         ELabel x_ae2g -> Just x_ae2g
         _ -> Nothing)

eSymbolicPredicate :: Iso (Predicate, ([Expr], (FormulaDisjunct, Pure))) Expr
eSymbolicPredicate =
  (Iso
     (\(x_ae2i, (x_ae2j, (x_ae2k, x_ae2l))) ->
        Just ((((ESymbolicPredicate x_ae2i) x_ae2j) x_ae2k) x_ae2l)))
    (\x_ae2m ->
       case x_ae2m of
         ESymbolicPredicate x_ae2i x_ae2j x_ae2k x_ae2l ->
           Just (x_ae2i, (x_ae2j, (x_ae2k, x_ae2l)))
         _ -> Nothing)

eFormulaDisjunct :: Iso [Formula] Expr
eFormulaDisjunct =
  (Iso (\x_ae2n -> Just (EFormulaDisjunct x_ae2n)))
    (\x_ae2o ->
       case x_ae2o of
         EFormulaDisjunct x_ae2n -> Just x_ae2n
         _ -> Nothing)

eFormulaExists :: Iso ([VarFirst], (Heap, Pure)) Expr
eFormulaExists =
  (Iso
     (\(x_ae2p, (x_ae2q, x_ae2r)) ->
        Just (((EFormulaExists x_ae2p) x_ae2q) x_ae2r)))
    (\x_ae2s ->
       case x_ae2s of
         EFormulaExists x_ae2p x_ae2q x_ae2r -> Just (x_ae2p, (x_ae2q, x_ae2r))
         _ -> Nothing)

eHeapEmp :: Iso () Expr
eHeapEmp =
  (Iso (\() -> Just EHeapEmp))
    (\x_ae2t ->
       case x_ae2t of
         EHeapEmp -> Just ()
         _ -> Nothing)

eHeapMap :: Iso (VarFirst, (DataStructure, [VarFirst])) Expr
eHeapMap =
  (Iso (\(x_ae2u, (x_ae2v, x_ae2w)) -> Just (((EHeapMap x_ae2u) x_ae2v) x_ae2w)))
    (\x_ae2x ->
       case x_ae2x of
         EHeapMap x_ae2u x_ae2v x_ae2w -> Just (x_ae2u, (x_ae2v, x_ae2w))
         _ -> Nothing)

eHeapPredicate :: Iso (Predicate, [Expr]) Expr
eHeapPredicate =
  (Iso (\(x_ae2y, x_ae2z) -> Just ((EHeapPredicate x_ae2y) x_ae2z)))
    (\x_ae2A ->
       case x_ae2A of
         EHeapPredicate x_ae2y x_ae2z -> Just (x_ae2y, x_ae2z)
         _ -> Nothing)

eBool :: Iso Bool Expr
eBool =
  (Iso (\x_ae2B -> Just (EBool x_ae2B)))
    (\x_ae2C ->
       case x_ae2C of
         EBool x_ae2B -> Just x_ae2B
         _ -> Nothing)

eInteger :: Iso Integer Expr
eInteger =
  (Iso (\x_ae3p -> Just (EInteger x_ae3p)))
    (\x_ae3q ->
       case x_ae3q of
         EInteger x_ae3p -> Just x_ae3p
         _ -> Nothing)

eGlobalProtocolTransmission ::
     Iso (Role, (Label, (Role, (Channel, (VarFirst, Formula))))) Expr
eGlobalProtocolTransmission =
  (Iso
     (\(x_ae2D, (x_ae2E, (x_ae2F, (x_ae2G, (x_ae2H, x_ae2I))))) ->
        Just
          ((((((EGlobalProtocolTransmission x_ae2D) x_ae2E) x_ae2F) x_ae2G)
              x_ae2H)
             x_ae2I)))
    (\x_ae2J ->
       case x_ae2J of
         EGlobalProtocolTransmission x_ae2D x_ae2E x_ae2F x_ae2G x_ae2H x_ae2I ->
           Just (x_ae2D, (x_ae2E, (x_ae2F, (x_ae2G, (x_ae2H, x_ae2I)))))
         _ -> Nothing)

eGlobalProtocolEmp :: Iso () Expr
eGlobalProtocolEmp =
  (Iso (\() -> Just EGlobalProtocolEmp))
    (\x_ae2K ->
       case x_ae2K of
         EGlobalProtocolEmp -> Just ()
         _ -> Nothing)

eEvent :: Iso (Role, Label) Expr
eEvent =
  (Iso (\(x_ae2L, x_ae2M) -> Just ((EEvent x_ae2L) x_ae2M)))
    (\x_ae2N ->
       case x_ae2N of
         EEvent x_ae2L x_ae2M -> Just (x_ae2L, x_ae2M)
         _ -> Nothing)

eConstraintCommunicates :: Iso (Event, Event) Expr
eConstraintCommunicates =
  (Iso (\(x_ae2O, x_ae2P) -> Just ((EConstraintCommunicates x_ae2O) x_ae2P)))
    (\x_ae2Q ->
       case x_ae2Q of
         EConstraintCommunicates x_ae2O x_ae2P -> Just (x_ae2O, x_ae2P)
         _ -> Nothing)

eConstraintHappens :: Iso (Event, Event) Expr
eConstraintHappens =
  (Iso (\(x_ae2R, x_ae2S) -> Just ((EConstraintHappens x_ae2R) x_ae2S)))
    (\x_ae2T ->
       case x_ae2T of
         EConstraintHappens x_ae2R x_ae2S -> Just (x_ae2R, x_ae2S)
         _ -> Nothing)

eAssertionEvent :: Iso Event Expr
eAssertionEvent =
  (Iso (\x_ae2U -> Just (EAssertionEvent x_ae2U)))
    (\x_ae2V ->
       case x_ae2V of
         EAssertionEvent x_ae2U -> Just x_ae2U
         _ -> Nothing)

eAssertionNEvent :: Iso Event Expr
eAssertionNEvent =
  (Iso (\x_ae2W -> Just (EAssertionNEvent x_ae2W)))
    (\x_ae2X ->
       case x_ae2X of
         EAssertionNEvent x_ae2W -> Just x_ae2W
         _ -> Nothing)

eAssertionConstraint :: Iso Constraint Expr
eAssertionConstraint =
  (Iso (\x_ae2Y -> Just (EAssertionConstraint x_ae2Y)))
    (\x_ae2Z ->
       case x_ae2Z of
         EAssertionConstraint x_ae2Y -> Just x_ae2Y
         _ -> Nothing)

eAssertionAnd :: Iso (Assertion, Assertion) Expr
eAssertionAnd =
  (Iso (\(x_ae30, x_ae31) -> Just ((EAssertionAnd x_ae30) x_ae31)))
    (\x_ae32 ->
       case x_ae32 of
         EAssertionAnd x_ae30 x_ae31 -> Just (x_ae30, x_ae31)
         _ -> Nothing)

eAssertionImplies :: Iso (Event, Assertion) Expr
eAssertionImplies =
  (Iso (\(x_ae33, x_ae34) -> Just ((EAssertionImplies x_ae33) x_ae34)))
    (\x_ae35 ->
       case x_ae35 of
         EAssertionImplies x_ae33 x_ae34 -> Just (x_ae33, x_ae34)
         _ -> Nothing)

ePartyProtocolSend :: Iso (Channel, (Label, (VarFirst, Formula))) Expr
ePartyProtocolSend =
  (Iso
     (\(x_ae36, (x_ae37, (x_ae38, x_ae39))) ->
        Just ((((EPartyProtocolSend x_ae36) x_ae37) x_ae38) x_ae39)))
    (\x_ae3a ->
       case x_ae3a of
         EPartyProtocolSend x_ae36 x_ae37 x_ae38 x_ae39 ->
           Just (x_ae36, (x_ae37, (x_ae38, x_ae39)))
         _ -> Nothing)

ePartyProtocolReceive :: Iso (Channel, (Label, (VarFirst, Formula))) Expr
ePartyProtocolReceive =
  (Iso
     (\(x_ae3b, (x_ae3c, (x_ae3d, x_ae3e))) ->
        Just ((((EPartyProtocolReceive x_ae3b) x_ae3c) x_ae3d) x_ae3e)))
    (\x_ae3f ->
       case x_ae3f of
         EPartyProtocolReceive x_ae3b x_ae3c x_ae3d x_ae3e ->
           Just (x_ae3b, (x_ae3c, (x_ae3d, x_ae3e)))
         _ -> Nothing)

ePartyProtocolConcurrency :: Iso (PartyProtocol, PartyProtocol) Expr
ePartyProtocolConcurrency =
  (Iso (\(x_ae3g, x_ae3h) -> Just ((EPartyProtocolConcurrency x_ae3g) x_ae3h)))
    (\x_ae3i ->
       case x_ae3i of
         EPartyProtocolConcurrency x_ae3g x_ae3h -> Just (x_ae3g, x_ae3h)
         _ -> Nothing)

ePartyProtocolChoice :: Iso (PartyProtocol, PartyProtocol) Expr
ePartyProtocolChoice =
  (Iso (\(x_ae3j, x_ae3k) -> Just ((EPartyProtocolChoice x_ae3j) x_ae3k)))
    (\x_ae3l ->
       case x_ae3l of
         EPartyProtocolChoice x_ae3j x_ae3k -> Just (x_ae3j, x_ae3k)
         _ -> Nothing)

ePartyProtocolSequencing :: Iso (PartyProtocol, PartyProtocol) Expr
ePartyProtocolSequencing =
  (Iso (\(x_ae3m, x_ae3n) -> Just ((EPartyProtocolSequencing x_ae3m) x_ae3n)))
    (\x_ae3o ->
       case x_ae3o of
         EPartyProtocolSequencing x_ae3m x_ae3n -> Just (x_ae3m, x_ae3n)
         _ -> Nothing)

ePartyProtocolAssumption :: Iso Assertion Expr
ePartyProtocolAssumption =
  (Iso (\x_ae3p -> Just (EPartyProtocolAssumption x_ae3p)))
    (\x_ae3q ->
       case x_ae3q of
         EPartyProtocolAssumption x_ae3p -> Just x_ae3p
         _ -> Nothing)

ePartyProtocolGuard :: Iso Assertion Expr
ePartyProtocolGuard =
  (Iso (\x_ae3r -> Just (EPartyProtocolGuard x_ae3r)))
    (\x_ae3s ->
       case x_ae3s of
         EPartyProtocolGuard x_ae3r -> Just x_ae3r
         _ -> Nothing)

ePartyProtocolEmp :: Iso () Expr
ePartyProtocolEmp =
  (Iso (\() -> Just EPartyProtocolEmp))
    (\x_ae3t ->
       case x_ae3t of
         EPartyProtocolEmp -> Just ()
         _ -> Nothing)

eEndpointProtocolSend :: Iso (Channel, (Label, (VarFirst, Formula))) Expr
eEndpointProtocolSend =
  (Iso
     (\(x_ae3u, (x_ae3v, (x_ae3w, x_ae3x))) ->
        Just ((((EEndpointProtocolSend x_ae3u) x_ae3v) x_ae3w) x_ae3x)))
    (\x_ae3y ->
       case x_ae3y of
         EEndpointProtocolSend x_ae3u x_ae3v x_ae3w x_ae3x ->
           Just (x_ae3u, (x_ae3v, (x_ae3w, x_ae3x)))
         _ -> Nothing)

eEndpointProtocolReceive :: Iso (Channel, (Label, (VarFirst, Formula))) Expr
eEndpointProtocolReceive =
  (Iso
     (\(x_ae3z, (x_ae3A, (x_ae3B, x_ae3C))) ->
        Just ((((EEndpointProtocolReceive x_ae3z) x_ae3A) x_ae3B) x_ae3C)))
    (\x_ae3D ->
       case x_ae3D of
         EEndpointProtocolReceive x_ae3z x_ae3A x_ae3B x_ae3C ->
           Just (x_ae3z, (x_ae3A, (x_ae3B, x_ae3C)))
         _ -> Nothing)

eEndpointProtocolConcurrency :: Iso (EndpointProtocol, EndpointProtocol) Expr
eEndpointProtocolConcurrency =
  (Iso
     (\(x_ae3E, x_ae3F) -> Just ((EEndpointProtocolConcurrency x_ae3E) x_ae3F)))
    (\x_ae3G ->
       case x_ae3G of
         EEndpointProtocolConcurrency x_ae3E x_ae3F -> Just (x_ae3E, x_ae3F)
         _ -> Nothing)

eEndpointProtocolChoice :: Iso (EndpointProtocol, EndpointProtocol) Expr
eEndpointProtocolChoice =
  (Iso (\(x_ae3H, x_ae3I) -> Just ((EEndpointProtocolChoice x_ae3H) x_ae3I)))
    (\x_ae3J ->
       case x_ae3J of
         EEndpointProtocolChoice x_ae3H x_ae3I -> Just (x_ae3H, x_ae3I)
         _ -> Nothing)

eEndpointProtocolSequencing :: Iso (EndpointProtocol, EndpointProtocol) Expr
eEndpointProtocolSequencing =
  (Iso (\(x_ae3K, x_ae3L) -> Just ((EEndpointProtocolSequencing x_ae3K) x_ae3L)))
    (\x_ae3M ->
       case x_ae3M of
         EEndpointProtocolSequencing x_ae3K x_ae3L -> Just (x_ae3K, x_ae3L)
         _ -> Nothing)

eEndpointProtocolAssumption :: Iso Assertion Expr
eEndpointProtocolAssumption =
  (Iso (\x_ae3N -> Just (EEndpointProtocolAssumption x_ae3N)))
    (\x_ae3O ->
       case x_ae3O of
         EEndpointProtocolAssumption x_ae3N -> Just x_ae3N
         _ -> Nothing)

eEndpointProtocolGuard :: Iso Assertion Expr
eEndpointProtocolGuard =
  (Iso (\x_ae3P -> Just (EEndpointProtocolGuard x_ae3P)))
    (\x_ae3Q ->
       case x_ae3Q of
         EEndpointProtocolGuard x_ae3P -> Just x_ae3P
         _ -> Nothing)

eEndpointProtocolEmp :: Iso () Expr
eEndpointProtocolEmp =
  (Iso (\() -> Just EEndpointProtocolEmp))
    (\x_ae3R ->
       case x_ae3R of
         EEndpointProtocolEmp -> Just ()
         _ -> Nothing)

eChannelProtocolTransmission ::
     Iso (Role, (Label, (Role, (VarFirst, Formula)))) Expr
eChannelProtocolTransmission =
  (Iso
     (\(x_ae3S, (x_ae3T, (x_ae3U, (x_ae3V, x_ae3W)))) ->
        Just
          (((((EChannelProtocolTransmission x_ae3S) x_ae3T) x_ae3U) x_ae3V)
             x_ae3W)))
    (\x_ae3X ->
       case x_ae3X of
         EChannelProtocolTransmission x_ae3S x_ae3T x_ae3U x_ae3V x_ae3W ->
           Just (x_ae3S, (x_ae3T, (x_ae3U, (x_ae3V, x_ae3W))))
         _ -> Nothing)

eChannelProtocolChoice :: Iso (ChannelProtocol, ChannelProtocol) Expr
eChannelProtocolChoice =
  (Iso (\(x_ae3Y, x_ae3Z) -> Just ((EChannelProtocolChoice x_ae3Y) x_ae3Z)))
    (\x_ae40 ->
       case x_ae40 of
         EChannelProtocolChoice x_ae3Y x_ae3Z -> Just (x_ae3Y, x_ae3Z)
         _ -> Nothing)

eChannelProtocolSequencing :: Iso (ChannelProtocol, ChannelProtocol) Expr
eChannelProtocolSequencing =
  (Iso (\(x_ae41, x_ae42) -> Just ((EChannelProtocolSequencing x_ae41) x_ae42)))
    (\x_ae43 ->
       case x_ae43 of
         EChannelProtocolSequencing x_ae41 x_ae42 -> Just (x_ae41, x_ae42)
         _ -> Nothing)

eChannelProtocolAssumption :: Iso Assertion Expr
eChannelProtocolAssumption =
  (Iso (\x_ae44 -> Just (EChannelProtocolAssumption x_ae44)))
    (\x_ae45 ->
       case x_ae45 of
         EChannelProtocolAssumption x_ae44 -> Just x_ae44
         _ -> Nothing)

eChannelProtocolGuard :: Iso Assertion Expr
eChannelProtocolGuard =
  (Iso (\x_ae46 -> Just (EChannelProtocolGuard x_ae46)))
    (\x_ae47 ->
       case x_ae47 of
         EChannelProtocolGuard x_ae46 -> Just x_ae46
         _ -> Nothing)

eChannelProtocolEmp :: Iso () Expr
eChannelProtocolEmp =
  (Iso (\() -> Just EChannelProtocolEmp))
    (\x_ae48 ->
       case x_ae48 of
         EChannelProtocolEmp -> Just ()
         _ -> Nothing)

eOpUnary :: Iso (OpUnary, Expr) Expr
eOpUnary =
  (Iso (\(x_ae4C, x_ae4D) -> Just ((EOpUnary x_ae4C) x_ae4D)))
    (\x_ae4E ->
       case x_ae4E of
         EOpUnary x_ae4C x_ae4D -> Just (x_ae4C, x_ae4D)
         _ -> Nothing)

eOpBinary :: Iso (Expr, (OpBinary, Expr)) Expr
eOpBinary =
  (Iso
     (\(x_ae49, (x_ae4a, x_ae4b)) -> Just (((EOpBinary x_ae49) x_ae4a) x_ae4b)))
    (\x_ae4c ->
       case x_ae4c of
         EOpBinary x_ae49 x_ae4a x_ae4b -> Just (x_ae49, (x_ae4a, x_ae4b))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpUnary)
ePureBool :: Iso () OpUnary
ePureBool =
  (Iso (\() -> Just EPureBool))
    (\x_aeix ->
       case x_aeix of
         EPureBool -> Just ()
         _ -> Nothing)

ePureNot :: Iso () OpUnary
ePureNot =
  (Iso (\() -> Just EPureNot))
    (\x_aeiy ->
       case x_aeiy of
         EPureNot -> Just ()
         _ -> Nothing)

ePureBoolInteger :: Iso () OpUnary
ePureBoolInteger =
  (Iso (\() -> Just EPureBoolInteger))
    (\x_aeiz ->
       case x_aeiz of
         EPureBoolInteger -> Just ()
         _ -> Nothing)

ePurePointer :: Iso () OpUnary
ePurePointer =
  (Iso (\() -> Just EPurePointer))
    (\x_aeiA ->
       case x_aeiA of
         EPurePointer -> Just ()
         _ -> Nothing)

ePointerNull :: Iso () OpUnary
ePointerNull =
  (Iso (\() -> Just EPointerNull))
    (\x_aeiB ->
       case x_aeiB of
         EPointerNull -> Just ()
         _ -> Nothing)

ePointerNNull :: Iso () OpUnary
ePointerNNull =
  (Iso (\() -> Just EPointerNNull))
    (\x_aeiC ->
       case x_aeiC of
         EPointerNNull -> Just ()
         _ -> Nothing)

eIntegerNeg :: Iso () OpUnary
eIntegerNeg =
  (Iso (\() -> Just EIntegerNeg))
    (\x_aeiD ->
       case x_aeiD of
         EIntegerNeg -> Just ()
         _ -> Nothing)

eIntegerVarFirst :: Iso () OpUnary
eIntegerVarFirst =
  (Iso (\() -> Just EIntegerVarFirst))
    (\x_aeiE ->
       case x_aeiE of
         EIntegerVarFirst -> Just ()
         _ -> Nothing)

eGlobalProtocolAssumption :: Iso () OpUnary
eGlobalProtocolAssumption =
  (Iso (\() -> Just EGlobalProtocolAssumption))
    (\x_aeiF ->
       case x_aeiF of
         EGlobalProtocolAssumption -> Just ()
         _ -> Nothing)

eGlobalProtocolGuard :: Iso () OpUnary
eGlobalProtocolGuard =
  (Iso (\() -> Just EGlobalProtocolGuard))
    (\x_aeiG ->
       case x_aeiG of
         EGlobalProtocolGuard -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''OpBinary)
eHeapSeparate :: Iso () OpBinary
eHeapSeparate =
  (Iso (\() -> Just EHeapSeparate))
    (\x_aele ->
       case x_aele of
         EHeapSeparate -> Just ()
         _ -> Nothing)

eFormulaSeparate :: Iso () OpBinary
eFormulaSeparate =
  (Iso (\() -> Just EFormulaSeparate))
    (\x_aelf ->
       case x_aelf of
         EFormulaSeparate -> Just ()
         _ -> Nothing)

ePureAnd :: Iso () OpBinary
ePureAnd =
  (Iso (\() -> Just EPureAnd))
    (\x_aelg ->
       case x_aelg of
         EPureAnd -> Just ()
         _ -> Nothing)

ePureOr :: Iso () OpBinary
ePureOr =
  (Iso (\() -> Just EPureOr))
    (\x_aelh ->
       case x_aelh of
         EPureOr -> Just ()
         _ -> Nothing)

ePureVarType :: Iso () OpBinary
ePureVarType =
  (Iso (\() -> Just EPureVarType))
    (\x_aeli ->
       case x_aeli of
         EPureVarType -> Just ()
         _ -> Nothing)

ePureExists :: Iso () OpBinary
ePureExists =
  (Iso (\() -> Just EPureExists))
    (\x_aelj ->
       case x_aelj of
         EPureExists -> Just ()
         _ -> Nothing)

ePureForall :: Iso () OpBinary
ePureForall =
  (Iso (\() -> Just EPureForall))
    (\x_aelk ->
       case x_aelk of
         EPureForall -> Just ()
         _ -> Nothing)

ePointerEq :: Iso () OpBinary
ePointerEq =
  (Iso (\() -> Just EPointerEq))
    (\x_aell ->
       case x_aell of
         EPointerEq -> Just ()
         _ -> Nothing)

ePointerNEq :: Iso () OpBinary
ePointerNEq =
  (Iso (\() -> Just EPointerNEq))
    (\x_aelm ->
       case x_aelm of
         EPointerNEq -> Just ()
         _ -> Nothing)

eBoolEq :: Iso () OpBinary
eBoolEq =
  (Iso (\() -> Just EBoolEq))
    (\x_aeln ->
       case x_aeln of
         EBoolEq -> Just ()
         _ -> Nothing)

eBoolIntegerEq :: Iso () OpBinary
eBoolIntegerEq =
  (Iso (\() -> Just EBoolIntegerEq))
    (\x_aelo ->
       case x_aelo of
         EBoolIntegerEq -> Just ()
         _ -> Nothing)

eBoolIntegerLeq :: Iso () OpBinary
eBoolIntegerLeq =
  (Iso (\() -> Just EBoolIntegerLeq))
    (\x_aelp ->
       case x_aelp of
         EBoolIntegerLeq -> Just ()
         _ -> Nothing)

eIntegerMul :: Iso () OpBinary
eIntegerMul =
  (Iso (\() -> Just EIntegerMul))
    (\x_aelq ->
       case x_aelq of
         EIntegerMul -> Just ()
         _ -> Nothing)

eIntegerAdd :: Iso () OpBinary
eIntegerAdd =
  (Iso (\() -> Just EIntegerAdd))
    (\x_aelr ->
       case x_aelr of
         EIntegerAdd -> Just ()
         _ -> Nothing)

eGlobalProtocolConcurrency :: Iso () OpBinary
eGlobalProtocolConcurrency =
  (Iso (\() -> Just EGlobalProtocolConcurrency))
    (\x_aels ->
       case x_aels of
         EGlobalProtocolConcurrency -> Just ()
         _ -> Nothing)

eGlobalProtocolChoice :: Iso () OpBinary
eGlobalProtocolChoice =
  (Iso (\() -> Just EGlobalProtocolChoice))
    (\x_aelt ->
       case x_aelt of
         EGlobalProtocolChoice -> Just ()
         _ -> Nothing)

eGlobalProtocolSequencing :: Iso () OpBinary
eGlobalProtocolSequencing =
  (Iso (\() -> Just EGlobalProtocolSequencing))
    (\x_aelu ->
       case x_aelu of
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
    exp 0 = eBool <$> bool
    exp 1 = chainl1 (exp 0) spacedOpBoolBinary (opPrioBoolBinary 1)
    opPrioBoolBinary n =
      eOpBinary . subset (\(_, (op, _)) -> prioBoolBinary op == n)

parseInteger :: Syntax delta => delta Expr
parseInteger = exp 2
  where
    exp 0 =
      eOpUnary <$> (opIntegerUnary <*> (eInteger <$> integer)) <|>
      eInteger <$> integer <|>
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
