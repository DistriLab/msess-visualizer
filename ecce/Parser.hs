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
{- pred ::= p(root,v*) = Φ inv π -}
data SymbolicPredicate =
  ESymbolicPredicate Predicate
                     [Expr]
                     FormulaDisjunct
                     Pure
  deriving (Show)

{- Φ ::= |Δ -}
data FormulaDisjunct =
  EFormulaDisjunct [Formula]
  deriving (Show)

{- Δ ::= ∃v*.κ^π | Δ*Δ -}
data Formula
  = EFormulaExists [VarFirst]
                   Heap
                   Pure
  | EFormulaSeparate Formula
                     Formula
  deriving (Show)

{- κ ::= emp | v↦d<v*> | p(v*) | κ*κ -}
data Heap
  = EHeapEmp
  | EHeapMap VarFirst
             DataStructure
             [VarFirst]
  | EHeapPredicate Predicate
                   [Expr]
  | EHeapSeparate Heap
                  Heap
  deriving (Show)

{- π ::= v:t | b | a | π^π | π|π | ~π | ∃v.π | ∀v.π | γ -}
data Pure
  = EPureVarType VarFirst
                 VarType
  | EPureBool Boole
  | EPureBoolInteger BoolInteger
  | EPureNot Pure
  | EPureExists VarFirst
                VarType
  | EPureForall VarFirst
                VarType
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
  = EBool Bool
  | EOpBoolBinary Boole
                  OpBoolBinary
                  Boole
  deriving (Show)

data OpBoolBinary =
  EBoolEq
  deriving (Show)

{- a ::= s=s | s<=s -}
data BoolInteger
  = EBoolIntegerEq Integer
                   Integer
  | EBoolIntegerLeq Integer
                    Integer
  deriving (Show)

{- s ::= k | v | k x s | s + s | -s -}
data Presburger
  = EInteger Integer
  | EIntegerVarFirst VarFirst
  | EIntegerNeg Presburger
  | EOpIntegerBinary Presburger
                     OpIntegerBinary
                     Presburger
  deriving (Show)

data OpIntegerBinary
  = EIntegerMul
  | EIntegerAdd
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
  | EGlobalProtocolConcurrency GlobalProtocol
                               GlobalProtocol
  | EGlobalProtocolChoice GlobalProtocol
                          GlobalProtocol
  | EGlobalProtocolSequencing GlobalProtocol
                              GlobalProtocol
  | EGlobalProtocolAssumption Assertion
  | EGlobalProtocolGuard Assertion
  | EGlobalProtocolEmp
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
  | EAssertionAnd Assertion
                  Assertion
  | EAssertionImplies Event
                      Assertion
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
  | EPartyProtocolConcurrency PartyProtocol
                              PartyProtocol
  | EPartyProtocolChoice PartyProtocol
                         PartyProtocol
  | EPartyProtocolSequencing PartyProtocol
                             PartyProtocol
  | EPartyProtocolAssumption Assertion
  | EPartyProtocolGuard Assertion
  | EPartyProtocolEmp
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
  | EEndpointProtocolConcurrency EndpointProtocol
                                 EndpointProtocol
  | EEndpointProtocolChoice EndpointProtocol
                            EndpointProtocol
  | EEndpointProtocolSequencing EndpointProtocol
                                EndpointProtocol
  | EEndpointProtocolAssumption Assertion
  | EEndpointProtocolGuard Assertion
  | EEndpointProtocolEmp
  deriving (Show)

{- Z ::= P--(i)->P:v.Δ | Z|Z | Z;Z | (-)(Ψ) | (+)(Ψ) -}
data ChannelProtocol
  = EChannelProtocolTransmission Role
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
  deriving (Show)

{- SUBSECTION EXPR -}
data Expr =
  GlobalProtocol
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

{-
 - SECTION SPLICE
 - TODO splice templates properly
 -}
-- $(defineIsomorphisms ''SymbolicPredicate)
eSymbolicPredicate ::
     Iso (Predicate, ([Expr], (FormulaDisjunct, Pure))) SymbolicPredicate
eSymbolicPredicate =
  (Iso
     (\(x_aea5, (x_aea6, (x_aea7, x_aea8))) ->
        Just ((((ESymbolicPredicate x_aea5) x_aea6) x_aea7) x_aea8)))
    (\x_aea9 ->
       case x_aea9 of
         ESymbolicPredicate x_aea5 x_aea6 x_aea7 x_aea8 ->
           Just (x_aea5, (x_aea6, (x_aea7, x_aea8))))

-- $(defineIsomorphisms ''FormulaDisjunct)
eFormulaDisjunct :: Iso [Formula] FormulaDisjunct
eFormulaDisjunct =
  (Iso (\x_aeaH -> Just (EFormulaDisjunct x_aeaH)))
    (\x_aeaI ->
       case x_aeaI of
         EFormulaDisjunct x_aeaH -> Just x_aeaH)

-- $(defineIsomorphisms ''Formula)
eFormulaExists :: Iso ([VarFirst], (Heap, Pure)) Formula
eFormulaExists =
  (Iso
     (\(x_aeb4, (x_aeb5, x_aeb6)) ->
        Just (((EFormulaExists x_aeb4) x_aeb5) x_aeb6)))
    (\x_aeb7 ->
       case x_aeb7 of
         EFormulaExists x_aeb4 x_aeb5 x_aeb6 -> Just (x_aeb4, (x_aeb5, x_aeb6))
         _ -> Nothing)

eFormulaSeparate :: Iso (Formula, Formula) Formula
eFormulaSeparate =
  (Iso (\(x_aeb8, x_aeb9) -> Just ((EFormulaSeparate x_aeb8) x_aeb9)))
    (\x_aeba ->
       case x_aeba of
         EFormulaSeparate x_aeb8 x_aeb9 -> Just (x_aeb8, x_aeb9)
         _ -> Nothing)

-- $(defineIsomorphisms ''Heap)
eHeapEmp :: Iso () Heap
eHeapEmp =
  (Iso (\() -> Just EHeapEmp))
    (\x_aebY ->
       case x_aebY of
         EHeapEmp -> Just ()
         _ -> Nothing)

eHeapMap :: Iso (VarFirst, (DataStructure, [VarFirst])) Heap
eHeapMap =
  (Iso (\(x_aebZ, (x_aec0, x_aec1)) -> Just (((EHeapMap x_aebZ) x_aec0) x_aec1)))
    (\x_aec2 ->
       case x_aec2 of
         EHeapMap x_aebZ x_aec0 x_aec1 -> Just (x_aebZ, (x_aec0, x_aec1))
         _ -> Nothing)

eHeapPredicate :: Iso (Predicate, [Expr]) Heap
eHeapPredicate =
  (Iso (\(x_aec3, x_aec4) -> Just ((EHeapPredicate x_aec3) x_aec4)))
    (\x_aec5 ->
       case x_aec5 of
         EHeapPredicate x_aec3 x_aec4 -> Just (x_aec3, x_aec4)
         _ -> Nothing)

eHeapSeparate :: Iso (Heap, Heap) Heap
eHeapSeparate =
  (Iso (\(x_aec6, x_aec7) -> Just ((EHeapSeparate x_aec6) x_aec7)))
    (\x_aec8 ->
       case x_aec8 of
         EHeapSeparate x_aec6 x_aec7 -> Just (x_aec6, x_aec7)
         _ -> Nothing)

-- $(defineIsomorphisms ''Pure)
ePureVarType :: Iso (VarFirst, VarType) Pure
ePureVarType =
  (Iso (\(x_aedu, x_aedv) -> Just ((EPureVarType x_aedu) x_aedv)))
    (\x_aedw ->
       case x_aedw of
         EPureVarType x_aedu x_aedv -> Just (x_aedu, x_aedv)
         _ -> Nothing)

ePureBool :: Iso Boole Pure
ePureBool =
  (Iso (\x_aedx -> Just (EPureBool x_aedx)))
    (\x_aedy ->
       case x_aedy of
         EPureBool x_aedx -> Just x_aedx
         _ -> Nothing)

ePureBoolInteger :: Iso BoolInteger Pure
ePureBoolInteger =
  (Iso (\x_aedz -> Just (EPureBoolInteger x_aedz)))
    (\x_aedA ->
       case x_aedA of
         EPureBoolInteger x_aedz -> Just x_aedz
         _ -> Nothing)

ePureNot :: Iso Pure Pure
ePureNot =
  (Iso (\x_aedB -> Just (EPureNot x_aedB)))
    (\x_aedC ->
       case x_aedC of
         EPureNot x_aedB -> Just x_aedB
         _ -> Nothing)

ePureExists :: Iso (VarFirst, VarType) Pure
ePureExists =
  (Iso (\(x_aedD, x_aedE) -> Just ((EPureExists x_aedD) x_aedE)))
    (\x_aedF ->
       case x_aedF of
         EPureExists x_aedD x_aedE -> Just (x_aedD, x_aedE)
         _ -> Nothing)

ePureForall :: Iso (VarFirst, VarType) Pure
ePureForall =
  (Iso (\(x_aedG, x_aedH) -> Just ((EPureForall x_aedG) x_aedH)))
    (\x_aedI ->
       case x_aedI of
         EPureForall x_aedG x_aedH -> Just (x_aedG, x_aedH)
         _ -> Nothing)

ePurePointer :: Iso Pointer Pure
ePurePointer =
  (Iso (\x_aedJ -> Just (EPurePointer x_aedJ)))
    (\x_aedK ->
       case x_aedK of
         EPurePointer x_aedJ -> Just x_aedJ
         _ -> Nothing)

eOpPureBinary :: Iso (Pure, (OpPureBinary, Pure)) Pure
eOpPureBinary =
  (Iso
     (\(x_aedL, (x_aedM, x_aedN)) ->
        Just (((EOpPureBinary x_aedL) x_aedM) x_aedN)))
    (\x_aedO ->
       case x_aedO of
         EOpPureBinary x_aedL x_aedM x_aedN -> Just (x_aedL, (x_aedM, x_aedN))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpPureBinary)
ePureAnd :: Iso () OpPureBinary
ePureAnd =
  (Iso (\() -> Just EPureAnd))
    (\x_aege ->
       case x_aege of
         EPureAnd -> Just ()
         _ -> Nothing)

ePureOr :: Iso () OpPureBinary
ePureOr =
  (Iso (\() -> Just EPureOr))
    (\x_aegh ->
       case x_aegh of
         EPureOr -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''Pointer)
ePointerEq :: Iso (VarFirst, VarFirst) Pointer
ePointerEq =
  (Iso (\(x_aeh1, x_aeh2) -> Just ((EPointerEq x_aeh1) x_aeh2)))
    (\x_aeh3 ->
       case x_aeh3 of
         EPointerEq x_aeh1 x_aeh2 -> Just (x_aeh1, x_aeh2)
         _ -> Nothing)

ePointerNull :: Iso VarFirst Pointer
ePointerNull =
  (Iso (\x_aeh4 -> Just (EPointerNull x_aeh4)))
    (\x_aeh5 ->
       case x_aeh5 of
         EPointerNull x_aeh4 -> Just x_aeh4
         _ -> Nothing)

ePointerNEq :: Iso (VarFirst, VarFirst) Pointer
ePointerNEq =
  (Iso (\(x_aeh6, x_aeh7) -> Just ((EPointerNEq x_aeh6) x_aeh7)))
    (\x_aeh8 ->
       case x_aeh8 of
         EPointerNEq x_aeh6 x_aeh7 -> Just (x_aeh6, x_aeh7)
         _ -> Nothing)

ePointerNNull :: Iso VarFirst Pointer
ePointerNNull =
  (Iso (\x_aeh9 -> Just (EPointerNNull x_aeh9)))
    (\x_aeha ->
       case x_aeha of
         EPointerNNull x_aeh9 -> Just x_aeh9
         _ -> Nothing)

-- $(defineIsomorphisms ''Boole)
eBool :: Iso Bool Boole
eBool =
  (Iso (\x_aeio -> Just (EBool x_aeio)))
    (\x_aeip ->
       case x_aeip of
         EBool x_aeio -> Just x_aeio
         _ -> Nothing)

eOpBoolBinary :: Iso (Boole, (OpBoolBinary, Boole)) Boole
eOpBoolBinary =
  (Iso
     (\(x_aeiq, (x_aeir, x_aeis)) ->
        Just (((EOpBoolBinary x_aeiq) x_aeir) x_aeis)))
    (\x_aeit ->
       case x_aeit of
         EOpBoolBinary x_aeiq x_aeir x_aeis -> Just (x_aeiq, (x_aeir, x_aeis))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpBoolBinary)
eBoolEq :: Iso () OpBoolBinary
eBoolEq =
  (Iso (\() -> Just EBoolEq))
    (\x_aejd ->
       case x_aejd of
         EBoolEq -> Just ())

-- $(defineIsomorphisms ''BoolInteger)
eBoolIntegerEq :: Iso (Integer, Integer) BoolInteger
eBoolIntegerEq =
  (Iso (\(x_aejz, x_aejA) -> Just ((EBoolIntegerEq x_aejz) x_aejA)))
    (\x_aejB ->
       case x_aejB of
         EBoolIntegerEq x_aejz x_aejA -> Just (x_aejz, x_aejA)
         _ -> Nothing)

eBoolIntegerLeq :: Iso (Integer, Integer) BoolInteger
eBoolIntegerLeq =
  (Iso (\(x_aejC, x_aejD) -> Just ((EBoolIntegerLeq x_aejC) x_aejD)))
    (\x_aejE ->
       case x_aejE of
         EBoolIntegerLeq x_aejC x_aejD -> Just (x_aejC, x_aejD)
         _ -> Nothing)

-- $(defineIsomorphisms ''Presburger)
eInteger :: Iso Integer Presburger
eInteger =
  (Iso (\x_aeko -> Just (EInteger x_aeko)))
    (\x_aekp ->
       case x_aekp of
         EInteger x_aeko -> Just x_aeko
         _ -> Nothing)

eIntegerVarFirst :: Iso VarFirst Presburger
eIntegerVarFirst =
  (Iso (\x_aekq -> Just (EIntegerVarFirst x_aekq)))
    (\x_aekr ->
       case x_aekr of
         EIntegerVarFirst x_aekq -> Just x_aekq
         _ -> Nothing)

eIntegerNeg :: Iso Presburger Presburger
eIntegerNeg =
  (Iso (\x_aeks -> Just (EIntegerNeg x_aeks)))
    (\x_aekt ->
       case x_aekt of
         EIntegerNeg x_aeks -> Just x_aeks
         _ -> Nothing)

eOpIntegerBinary :: Iso (Presburger, (OpIntegerBinary, Presburger)) Presburger
eOpIntegerBinary =
  (Iso
     (\(x_aeku, (x_aekv, x_aekw)) ->
        Just (((EOpIntegerBinary x_aeku) x_aekv) x_aekw)))
    (\x_aekx ->
       case x_aekx of
         EOpIntegerBinary x_aeku x_aekv x_aekw ->
           Just (x_aeku, (x_aekv, x_aekw))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpIntegerBinary)
eIntegerMul :: Iso () OpIntegerBinary
eIntegerMul =
  (Iso (\() -> Just EIntegerMul))
    (\x_aelL ->
       case x_aelL of
         EIntegerMul -> Just ()
         _ -> Nothing)

eIntegerAdd :: Iso () OpIntegerBinary
eIntegerAdd =
  (Iso (\() -> Just EIntegerAdd))
    (\x_aelM ->
       case x_aelM of
         EIntegerAdd -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''GlobalProtocol)
eGlobalProtocolTransmission ::
     Iso (Role, (Label, (Role, (Channel, (VarFirst, Formula))))) GlobalProtocol
eGlobalProtocolTransmission =
  (Iso
     (\(x_aemo, (x_aemp, (x_aemq, (x_aemr, (x_aems, x_aemt))))) ->
        Just
          ((((((EGlobalProtocolTransmission x_aemo) x_aemp) x_aemq) x_aemr)
              x_aems)
             x_aemt)))
    (\x_aemu ->
       case x_aemu of
         EGlobalProtocolTransmission x_aemo x_aemp x_aemq x_aemr x_aems x_aemt ->
           Just (x_aemo, (x_aemp, (x_aemq, (x_aemr, (x_aems, x_aemt)))))
         _ -> Nothing)

eGlobalProtocolConcurrency ::
     Iso (GlobalProtocol, GlobalProtocol) GlobalProtocol
eGlobalProtocolConcurrency =
  (Iso (\(x_aemv, x_aemw) -> Just ((EGlobalProtocolConcurrency x_aemv) x_aemw)))
    (\x_aemx ->
       case x_aemx of
         EGlobalProtocolConcurrency x_aemv x_aemw -> Just (x_aemv, x_aemw)
         _ -> Nothing)

eGlobalProtocolChoice :: Iso (GlobalProtocol, GlobalProtocol) GlobalProtocol
eGlobalProtocolChoice =
  (Iso (\(x_aemy, x_aemz) -> Just ((EGlobalProtocolChoice x_aemy) x_aemz)))
    (\x_aemA ->
       case x_aemA of
         EGlobalProtocolChoice x_aemy x_aemz -> Just (x_aemy, x_aemz)
         _ -> Nothing)

eGlobalProtocolSequencing :: Iso (GlobalProtocol, GlobalProtocol) GlobalProtocol
eGlobalProtocolSequencing =
  (Iso (\(x_aemB, x_aemC) -> Just ((EGlobalProtocolSequencing x_aemB) x_aemC)))
    (\x_aemD ->
       case x_aemD of
         EGlobalProtocolSequencing x_aemB x_aemC -> Just (x_aemB, x_aemC)
         _ -> Nothing)

eGlobalProtocolAssumption :: Iso Assertion GlobalProtocol
eGlobalProtocolAssumption =
  (Iso (\x_aemE -> Just (EGlobalProtocolAssumption x_aemE)))
    (\x_aemF ->
       case x_aemF of
         EGlobalProtocolAssumption x_aemE -> Just x_aemE
         _ -> Nothing)

eGlobalProtocolGuard :: Iso Assertion GlobalProtocol
eGlobalProtocolGuard =
  (Iso (\x_aemG -> Just (EGlobalProtocolGuard x_aemG)))
    (\x_aemH ->
       case x_aemH of
         EGlobalProtocolGuard x_aemG -> Just x_aemG
         _ -> Nothing)

eGlobalProtocolEmp :: Iso () GlobalProtocol
eGlobalProtocolEmp =
  (Iso (\() -> Just EGlobalProtocolEmp))
    (\x_aemI ->
       case x_aemI of
         EGlobalProtocolEmp -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''Event)
eEvent :: Iso (Role, Label) Event
eEvent =
  (Iso (\(x_aep3, x_aep4) -> Just ((EEvent x_aep3) x_aep4)))
    (\x_aep5 ->
       case x_aep5 of
         EEvent x_aep3 x_aep4 -> Just (x_aep3, x_aep4))

-- $(defineIsomorphisms ''Constraint)
eConstraintCommunicates :: Iso (Event, Event) Constraint
eConstraintCommunicates =
  (Iso (\(x_aepv, x_aepw) -> Just ((EConstraintCommunicates x_aepv) x_aepw)))
    (\x_aepx ->
       case x_aepx of
         EConstraintCommunicates x_aepv x_aepw -> Just (x_aepv, x_aepw)
         _ -> Nothing)

eConstraintHappens :: Iso (Event, Event) Constraint
eConstraintHappens =
  (Iso (\(x_aepy, x_aepz) -> Just ((EConstraintHappens x_aepy) x_aepz)))
    (\x_aepA ->
       case x_aepA of
         EConstraintHappens x_aepy x_aepz -> Just (x_aepy, x_aepz)
         _ -> Nothing)

-- $(defineIsomorphisms ''Assertion)
eAssertionEvent :: Iso Event Assertion
eAssertionEvent =
  (Iso (\x_aeqk -> Just (EAssertionEvent x_aeqk)))
    (\x_aeql ->
       case x_aeql of
         EAssertionEvent x_aeqk -> Just x_aeqk
         _ -> Nothing)

eAssertionNEvent :: Iso Event Assertion
eAssertionNEvent =
  (Iso (\x_aeqm -> Just (EAssertionNEvent x_aeqm)))
    (\x_aeqn ->
       case x_aeqn of
         EAssertionNEvent x_aeqm -> Just x_aeqm
         _ -> Nothing)

eAssertionConstraint :: Iso Constraint Assertion
eAssertionConstraint =
  (Iso (\x_aeqo -> Just (EAssertionConstraint x_aeqo)))
    (\x_aeqp ->
       case x_aeqp of
         EAssertionConstraint x_aeqo -> Just x_aeqo
         _ -> Nothing)

eAssertionAnd :: Iso (Assertion, Assertion) Assertion
eAssertionAnd =
  (Iso (\(x_aeqq, x_aeqr) -> Just ((EAssertionAnd x_aeqq) x_aeqr)))
    (\x_aeqs ->
       case x_aeqs of
         EAssertionAnd x_aeqq x_aeqr -> Just (x_aeqq, x_aeqr)
         _ -> Nothing)

eAssertionImplies :: Iso (Event, Assertion) Assertion
eAssertionImplies =
  (Iso (\(x_aeqt, x_aequ) -> Just ((EAssertionImplies x_aeqt) x_aequ)))
    (\x_aeqv ->
       case x_aeqv of
         EAssertionImplies x_aeqt x_aequ -> Just (x_aeqt, x_aequ)
         _ -> Nothing)

-- $(defineIsomorphisms ''PartyProtocol)
ePartyProtocolSend :: Iso (Channel, (Label, (VarFirst, Formula))) PartyProtocol
ePartyProtocolSend =
  (Iso
     (\(x_aerY, (x_aerZ, (x_aes0, x_aes1))) ->
        Just ((((EPartyProtocolSend x_aerY) x_aerZ) x_aes0) x_aes1)))
    (\x_aes2 ->
       case x_aes2 of
         EPartyProtocolSend x_aerY x_aerZ x_aes0 x_aes1 ->
           Just (x_aerY, (x_aerZ, (x_aes0, x_aes1)))
         _ -> Nothing)

ePartyProtocolReceive ::
     Iso (Channel, (Label, (VarFirst, Formula))) PartyProtocol
ePartyProtocolReceive =
  (Iso
     (\(x_aes3, (x_aes4, (x_aes5, x_aes6))) ->
        Just ((((EPartyProtocolReceive x_aes3) x_aes4) x_aes5) x_aes6)))
    (\x_aes7 ->
       case x_aes7 of
         EPartyProtocolReceive x_aes3 x_aes4 x_aes5 x_aes6 ->
           Just (x_aes3, (x_aes4, (x_aes5, x_aes6)))
         _ -> Nothing)

ePartyProtocolConcurrency :: Iso (PartyProtocol, PartyProtocol) PartyProtocol
ePartyProtocolConcurrency =
  (Iso (\(x_aes8, x_aes9) -> Just ((EPartyProtocolConcurrency x_aes8) x_aes9)))
    (\x_aesa ->
       case x_aesa of
         EPartyProtocolConcurrency x_aes8 x_aes9 -> Just (x_aes8, x_aes9)
         _ -> Nothing)

ePartyProtocolChoice :: Iso (PartyProtocol, PartyProtocol) PartyProtocol
ePartyProtocolChoice =
  (Iso (\(x_aesb, x_aesc) -> Just ((EPartyProtocolChoice x_aesb) x_aesc)))
    (\x_aesd ->
       case x_aesd of
         EPartyProtocolChoice x_aesb x_aesc -> Just (x_aesb, x_aesc)
         _ -> Nothing)

ePartyProtocolSequencing :: Iso (PartyProtocol, PartyProtocol) PartyProtocol
ePartyProtocolSequencing =
  (Iso (\(x_aese, x_aesf) -> Just ((EPartyProtocolSequencing x_aese) x_aesf)))
    (\x_aesg ->
       case x_aesg of
         EPartyProtocolSequencing x_aese x_aesf -> Just (x_aese, x_aesf)
         _ -> Nothing)

ePartyProtocolAssumption :: Iso Assertion PartyProtocol
ePartyProtocolAssumption =
  (Iso (\x_aesh -> Just (EPartyProtocolAssumption x_aesh)))
    (\x_aesi ->
       case x_aesi of
         EPartyProtocolAssumption x_aesh -> Just x_aesh
         _ -> Nothing)

ePartyProtocolGuard :: Iso Assertion PartyProtocol
ePartyProtocolGuard =
  (Iso (\x_aesj -> Just (EPartyProtocolGuard x_aesj)))
    (\x_aesk ->
       case x_aesk of
         EPartyProtocolGuard x_aesj -> Just x_aesj
         _ -> Nothing)

ePartyProtocolEmp :: Iso () PartyProtocol
ePartyProtocolEmp =
  (Iso (\() -> Just EPartyProtocolEmp))
    (\x_aesl ->
       case x_aesl of
         EPartyProtocolEmp -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''EndpointProtocol)
eEndpointProtocolSend ::
     Iso (Channel, (Label, (VarFirst, Formula))) EndpointProtocol
eEndpointProtocolSend =
  (Iso
     (\(x_aeuZ, (x_aev0, (x_aev1, x_aev2))) ->
        Just ((((EEndpointProtocolSend x_aeuZ) x_aev0) x_aev1) x_aev2)))
    (\x_aev3 ->
       case x_aev3 of
         EEndpointProtocolSend x_aeuZ x_aev0 x_aev1 x_aev2 ->
           Just (x_aeuZ, (x_aev0, (x_aev1, x_aev2)))
         _ -> Nothing)

eEndpointProtocolReceive ::
     Iso (Channel, (Label, (VarFirst, Formula))) EndpointProtocol
eEndpointProtocolReceive =
  (Iso
     (\(x_aev4, (x_aev5, (x_aev6, x_aev7))) ->
        Just ((((EEndpointProtocolReceive x_aev4) x_aev5) x_aev6) x_aev7)))
    (\x_aev8 ->
       case x_aev8 of
         EEndpointProtocolReceive x_aev4 x_aev5 x_aev6 x_aev7 ->
           Just (x_aev4, (x_aev5, (x_aev6, x_aev7)))
         _ -> Nothing)

eEndpointProtocolConcurrency ::
     Iso (EndpointProtocol, EndpointProtocol) EndpointProtocol
eEndpointProtocolConcurrency =
  (Iso
     (\(x_aev9, x_aeva) -> Just ((EEndpointProtocolConcurrency x_aev9) x_aeva)))
    (\x_aevb ->
       case x_aevb of
         EEndpointProtocolConcurrency x_aev9 x_aeva -> Just (x_aev9, x_aeva)
         _ -> Nothing)

eEndpointProtocolChoice ::
     Iso (EndpointProtocol, EndpointProtocol) EndpointProtocol
eEndpointProtocolChoice =
  (Iso (\(x_aevc, x_aevd) -> Just ((EEndpointProtocolChoice x_aevc) x_aevd)))
    (\x_aeve ->
       case x_aeve of
         EEndpointProtocolChoice x_aevc x_aevd -> Just (x_aevc, x_aevd)
         _ -> Nothing)

eEndpointProtocolSequencing ::
     Iso (EndpointProtocol, EndpointProtocol) EndpointProtocol
eEndpointProtocolSequencing =
  (Iso (\(x_aevf, x_aevg) -> Just ((EEndpointProtocolSequencing x_aevf) x_aevg)))
    (\x_aevh ->
       case x_aevh of
         EEndpointProtocolSequencing x_aevf x_aevg -> Just (x_aevf, x_aevg)
         _ -> Nothing)

eEndpointProtocolAssumption :: Iso Assertion EndpointProtocol
eEndpointProtocolAssumption =
  (Iso (\x_aevi -> Just (EEndpointProtocolAssumption x_aevi)))
    (\x_aevj ->
       case x_aevj of
         EEndpointProtocolAssumption x_aevi -> Just x_aevi
         _ -> Nothing)

eEndpointProtocolGuard :: Iso Assertion EndpointProtocol
eEndpointProtocolGuard =
  (Iso (\x_aevk -> Just (EEndpointProtocolGuard x_aevk)))
    (\x_aevl ->
       case x_aevl of
         EEndpointProtocolGuard x_aevk -> Just x_aevk
         _ -> Nothing)

eEndpointProtocolEmp :: Iso () EndpointProtocol
eEndpointProtocolEmp =
  (Iso (\() -> Just EEndpointProtocolEmp))
    (\x_aevm ->
       case x_aevm of
         EEndpointProtocolEmp -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''ChannelProtocol)
eChannelProtocolTransmission ::
     Iso (Role, (Label, (Role, (VarFirst, Formula)))) ChannelProtocol
eChannelProtocolTransmission =
  (Iso
     (\(x_aey0, (x_aey1, (x_aey2, (x_aey3, x_aey4)))) ->
        Just
          (((((EChannelProtocolTransmission x_aey0) x_aey1) x_aey2) x_aey3)
             x_aey4)))
    (\x_aey5 ->
       case x_aey5 of
         EChannelProtocolTransmission x_aey0 x_aey1 x_aey2 x_aey3 x_aey4 ->
           Just (x_aey0, (x_aey1, (x_aey2, (x_aey3, x_aey4))))
         _ -> Nothing)

eChannelProtocolChoice :: Iso (ChannelProtocol, ChannelProtocol) ChannelProtocol
eChannelProtocolChoice =
  (Iso (\(x_aey6, x_aey7) -> Just ((EChannelProtocolChoice x_aey6) x_aey7)))
    (\x_aey8 ->
       case x_aey8 of
         EChannelProtocolChoice x_aey6 x_aey7 -> Just (x_aey6, x_aey7)
         _ -> Nothing)

eChannelProtocolSequencing ::
     Iso (ChannelProtocol, ChannelProtocol) ChannelProtocol
eChannelProtocolSequencing =
  (Iso (\(x_aey9, x_aeya) -> Just ((EChannelProtocolSequencing x_aey9) x_aeya)))
    (\x_aeyb ->
       case x_aeyb of
         EChannelProtocolSequencing x_aey9 x_aeya -> Just (x_aey9, x_aeya)
         _ -> Nothing)

eChannelProtocolAssumption :: Iso Assertion ChannelProtocol
eChannelProtocolAssumption =
  (Iso (\x_aeyc -> Just (EChannelProtocolAssumption x_aeyc)))
    (\x_aeyd ->
       case x_aeyd of
         EChannelProtocolAssumption x_aeyc -> Just x_aeyc
         _ -> Nothing)

eChannelProtocolGuard :: Iso Assertion ChannelProtocol
eChannelProtocolGuard =
  (Iso (\x_aeye -> Just (EChannelProtocolGuard x_aeye)))
    (\x_aeyf ->
       case x_aeyf of
         EChannelProtocolGuard x_aeye -> Just x_aeye
         _ -> Nothing)

eChannelProtocolEmp :: Iso () ChannelProtocol
eChannelProtocolEmp =
  (Iso (\() -> Just EChannelProtocolEmp))
    (\x_aeyg ->
       case x_aeyg of
         EChannelProtocolEmp -> Just ()
         _ -> Nothing)

{-
 - SECTION PARSER
 -}
{-
 - SUBSECTION HELPERS
 -}
parseExpr :: Syntax delta => delta Expr
parseExpr = parseGlobalProtocol

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
      text "~" *> (ePureNot <$> parsePure) <|> ePureBool <$> parseBool <|>
      parens (skipSpace *> parsePure <* skipSpace)
    exp 1 = chainl1 (exp 0) opPureBinary (opPrioPureBinary 1)
    exp 2 = chainl1 (exp 1) opPureBinary (opPrioPureBinary 2)
    opPrioPureBinary n =
      eOpPureBinary . subset (\(_, (op, _)) -> prioPureBinary op == n)

{-
 - SUBSECTION γ
 -}
{-
 - SUBSECTION b
 -}
opBoolBinary :: Syntax delta => delta OpBoolBinary
opBoolBinary = between optSpace optSpace (eBoolEq <$> text "=")

prioBoolBinary :: OpBoolBinary -> Integer
prioBoolBinary EBoolEq = 1

parseBool :: Syntax delta => delta Boole
parseBool = exp 1
  where
    exp 0 = eBool <$> bool
    exp 1 = chainl1 (exp 0) opBoolBinary (opPrioBoolBinary 1)
    opPrioBoolBinary n =
      eOpBoolBinary . subset (\(_, (op, _)) -> prioBoolBinary op == n)

{-
 - SUBSECTION a
 -}
{-
 - SUBSECTION s
 -}
opIntegerBinary :: Syntax delta => delta OpIntegerBinary
opIntegerBinary =
  between
    optSpace
    optSpace
    (eIntegerMul <$> text "*" <|> eIntegerAdd <$> text "+")

prioIntegerBinary :: OpIntegerBinary -> Integer
prioIntegerBinary EIntegerMul = 1
prioIntegerBinary EIntegerAdd = 2

parseInteger :: Syntax delta => delta Presburger
parseInteger = exp 2
  where
    exp 0 =
      text "-" *> (eIntegerNeg <$> parseInteger) <|> eInteger <$> integer <|>
      parens (skipSpace *> parseInteger <* skipSpace)
    exp 1 = chainl1 (exp 0) opIntegerBinary (opPrioIntegerBinary 1)
    exp 2 = chainl1 (exp 1) opIntegerBinary (opPrioIntegerBinary 2)
    opPrioIntegerBinary n =
      eOpIntegerBinary . subset (\(_, (op, _)) -> prioIntegerBinary op == n)
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
