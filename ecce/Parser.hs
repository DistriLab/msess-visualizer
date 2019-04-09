{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE StandaloneDeriving #-} -- Allows `deriving` on its own line
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns
{-# LANGUAGE EmptyDataDecls #-} -- Allows datatypes without constructors
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

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

{- SUBSECTION EXPR -}
data Expr
  {- Figure 2.2 -}
  {- pred ::= p(root,v*) = Φ inv π -}
  = ESymbolicPredicate Expr
                       Predicate
                       [Expr]
                       Expr
                       Expr
                       Expr
                       Expr
  {- Φ ::= |Δ -}
  | EFormulaDisjunct [Expr]
  {- Δ ::= ∃v*.κ^π | Δ*Δ -}
  | EFormulaExists [Expr]
                   Expr
                   Expr
                   Expr
                   Expr
  {- κ ::= emp | v↦d<v*> | p(v*) | κ*κ -}
  | EHeapMap Expr
             VarFirst
             Expr
             DataStructure
             [Expr]
  | EHeapPredicate Expr
                   Predicate
                   [Expr]
  {- Figure 4.1 -}
  {- G ::= S--(i)->R:c<v.Δ> | G*G | G|G | G;G | (+)(Ψ) | (-)(Ψ) | emp -}
  | EGlobalProtocolTransmission Expr
                                Role
                                Expr
                                Label
                                Expr
                                Role
                                Expr
                                Channel
                                Expr
                                VarFirst
                                Expr
                                Expr
  {- Figure 4.3 -}
  {- E ::= P(i) -}
  | EEvent Expr
           Role
           Expr
           Label
  {- ν ::= E<CBE | E<HBE -}
  | EConstraintCommunicates Expr
                            Expr
                            Expr
                            Expr
  | EConstraintHappens Expr
                       Expr
                       Expr
                       Expr
  {- Ψ ::= E | ~(E) | ν | Ψ^Ψ | E==>Ψ -}
  | EAssertionEvent Expr
                    Expr
  | EAssertionNEvent Expr
                     Expr
  | EAssertionConstraint Expr
                         Expr
  | EAssertionAnd Expr
                  Expr
                  Expr
                  Expr
  | EAssertionImplies Expr
                      Expr
                      Expr
                      Expr
  {- Figure 4.5 -}
  {- γ ::= c(i)!v.Δ | c(i)?v.Δ | γ*γ | γ|γ | γ;γ | (-)(Ψ) | (+)(Ψ) -}
  | EPartyProtocolSend Expr
                       Channel
                       Expr
                       Label
                       Expr
                       VarFirst
                       Expr
                       Expr
  | EPartyProtocolReceive Expr
                          Channel
                          Expr
                          Label
                          Expr
                          VarFirst
                          Expr
                          Expr
  | EPartyProtocolConcurrency Expr
                              Expr
                              Expr
                              Expr
  | EPartyProtocolChoice Expr
                         Expr
                         Expr
                         Expr
  | EPartyProtocolSequencing Expr
                             Expr
                             Expr
                             Expr
  | EPartyProtocolAssumption Expr
                             Expr
  | EPartyProtocolGuard Expr
                        Expr
  {- L ::= (i)!v.Δ | (i)?v.Δ | L|L | L;L | (-)(Ψ) | (+)(Ψ) -}
  | EEndpointProtocolSend Expr
                          Channel
                          Expr
                          Label
                          Expr
                          VarFirst
                          Expr
                          Expr
  | EEndpointProtocolReceive Expr
                             Channel
                             Expr
                             Label
                             Expr
                             VarFirst
                             Expr
                             Expr
  -- Note:
  --    We also define L ::= L*L.
  --    See Projector.hs, SUBSECTION PER PARTY SPEC -> PER ENDPOINT SPEC Note
  --    for more details.
  | EEndpointProtocolConcurrency Expr
                                 Expr
                                 Expr
                                 Expr
  | EEndpointProtocolChoice Expr
                            Expr
                            Expr
                            Expr
  | EEndpointProtocolSequencing Expr
                                Expr
                                Expr
                                Expr
  | EEndpointProtocolAssumption Expr
                                Expr
  | EEndpointProtocolGuard Expr
                           Expr
  {- Z ::= P--(i)->P:v.Δ | Z|Z | Z;Z | (-)(Ψ) | (+)(Ψ) -}
  | EChannelProtocolTransmission Expr
                                 Role
                                 Expr
                                 Label
                                 Expr
                                 Role
                                 Expr
                                 VarFirst
                                 Expr
                                 Expr
  | EChannelProtocolChoice Expr
                           Expr
                           Expr
                           Expr
  | EChannelProtocolSequencing Expr
                               Expr
                               Expr
                               Expr
  | EChannelProtocolAssumption Expr
                               Expr
  | EChannelProtocolGuard Expr
                          Expr
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
eSymbolicPredicate ::
     Iso (Expr, (Predicate, ([Expr], (Expr, (Expr, (Expr, Expr)))))) Expr
eSymbolicPredicate =
  (Iso
     (\(x_ae9L, (x_ae9M, (x_ae9N, (x_ae9O, (x_ae9P, (x_ae9Q, x_ae9R)))))) ->
        Just
          (((((((ESymbolicPredicate x_ae9L) x_ae9M) x_ae9N) x_ae9O) x_ae9P)
              x_ae9Q)
             x_ae9R)))
    (\x_ae9S ->
       case x_ae9S of
         ESymbolicPredicate x_ae9L x_ae9M x_ae9N x_ae9O x_ae9P x_ae9Q x_ae9R ->
           Just
             (x_ae9L, (x_ae9M, (x_ae9N, (x_ae9O, (x_ae9P, (x_ae9Q, x_ae9R))))))
         _ -> Nothing)

eFormulaDisjunct :: Iso [Expr] Expr
eFormulaDisjunct =
  (Iso (\x_ae9T -> Just (EFormulaDisjunct x_ae9T)))
    (\x_ae9U ->
       case x_ae9U of
         EFormulaDisjunct x_ae9T -> Just x_ae9T
         _ -> Nothing)

eFormulaExists :: Iso ([Expr], (Expr, (Expr, (Expr, Expr)))) Expr
eFormulaExists =
  (Iso
     (\(x_ae9V, (x_ae9W, (x_ae9X, (x_ae9Y, x_ae9Z)))) ->
        Just (((((EFormulaExists x_ae9V) x_ae9W) x_ae9X) x_ae9Y) x_ae9Z)))
    (\x_aea0 ->
       case x_aea0 of
         EFormulaExists x_ae9V x_ae9W x_ae9X x_ae9Y x_ae9Z ->
           Just (x_ae9V, (x_ae9W, (x_ae9X, (x_ae9Y, x_ae9Z))))
         _ -> Nothing)

eHeapMap :: Iso (Expr, (VarFirst, (Expr, (DataStructure, [Expr])))) Expr
eHeapMap =
  (Iso
     (\(x_aea1, (x_aea2, (x_aea3, (x_aea4, x_aea5)))) ->
        Just (((((EHeapMap x_aea1) x_aea2) x_aea3) x_aea4) x_aea5)))
    (\x_aea6 ->
       case x_aea6 of
         EHeapMap x_aea1 x_aea2 x_aea3 x_aea4 x_aea5 ->
           Just (x_aea1, (x_aea2, (x_aea3, (x_aea4, x_aea5))))
         _ -> Nothing)

eHeapPredicate :: Iso (Expr, (Predicate, [Expr])) Expr
eHeapPredicate =
  (Iso
     (\(x_aea7, (x_aea8, x_aea9)) ->
        Just (((EHeapPredicate x_aea7) x_aea8) x_aea9)))
    (\x_aeaa ->
       case x_aeaa of
         EHeapPredicate x_aea7 x_aea8 x_aea9 -> Just (x_aea7, (x_aea8, x_aea9))
         _ -> Nothing)

eGlobalProtocolTransmission ::
     Iso ( Expr
         , ( Role
           , ( Expr
             , ( Label
               , ( Expr
                 , (Role, (Expr, (Channel, (Expr, (VarFirst, (Expr, Expr))))))))))) Expr
eGlobalProtocolTransmission =
  (Iso
     (\(x_aeab, (x_aeac, (x_aead, (x_aeae, (x_aeaf, (x_aeag, (x_aeah, (x_aeai, (x_aeaj, (x_aeak, (x_aeal, x_aeam))))))))))) ->
        Just
          ((((((((((((EGlobalProtocolTransmission x_aeab) x_aeac) x_aead) x_aeae)
                    x_aeaf)
                   x_aeag)
                  x_aeah)
                 x_aeai)
                x_aeaj)
               x_aeak)
              x_aeal)
             x_aeam)))
    (\x_aean ->
       case x_aean of
         EGlobalProtocolTransmission x_aeab x_aeac x_aead x_aeae x_aeaf x_aeag x_aeah x_aeai x_aeaj x_aeak x_aeal x_aeam ->
           Just
             ( x_aeab
             , ( x_aeac
               , ( x_aead
                 , ( x_aeae
                   , ( x_aeaf
                     , ( x_aeag
                       , ( x_aeah
                         , (x_aeai, (x_aeaj, (x_aeak, (x_aeal, x_aeam)))))))))))
         _ -> Nothing)

eEvent :: Iso (Expr, (Role, (Expr, Label))) Expr
eEvent =
  (Iso
     (\(x_aeao, (x_aeap, (x_aeaq, x_aear))) ->
        Just ((((EEvent x_aeao) x_aeap) x_aeaq) x_aear)))
    (\x_aeas ->
       case x_aeas of
         EEvent x_aeao x_aeap x_aeaq x_aear ->
           Just (x_aeao, (x_aeap, (x_aeaq, x_aear)))
         _ -> Nothing)

eConstraintCommunicates :: Iso (Expr, (Expr, (Expr, Expr))) Expr
eConstraintCommunicates =
  (Iso
     (\(x_aeat, (x_aeau, (x_aeav, x_aeaw))) ->
        Just ((((EConstraintCommunicates x_aeat) x_aeau) x_aeav) x_aeaw)))
    (\x_aeax ->
       case x_aeax of
         EConstraintCommunicates x_aeat x_aeau x_aeav x_aeaw ->
           Just (x_aeat, (x_aeau, (x_aeav, x_aeaw)))
         _ -> Nothing)

eConstraintHappens :: Iso (Expr, (Expr, (Expr, Expr))) Expr
eConstraintHappens =
  (Iso
     (\(x_aeay, (x_aeaz, (x_aeaA, x_aeaB))) ->
        Just ((((EConstraintHappens x_aeay) x_aeaz) x_aeaA) x_aeaB)))
    (\x_aeaC ->
       case x_aeaC of
         EConstraintHappens x_aeay x_aeaz x_aeaA x_aeaB ->
           Just (x_aeay, (x_aeaz, (x_aeaA, x_aeaB)))
         _ -> Nothing)

eAssertionEvent :: Iso (Expr, Expr) Expr
eAssertionEvent =
  (Iso (\(x_aeaD, x_aeaE) -> Just ((EAssertionEvent x_aeaD) x_aeaE)))
    (\x_aeaF ->
       case x_aeaF of
         EAssertionEvent x_aeaD x_aeaE -> Just (x_aeaD, x_aeaE)
         _ -> Nothing)

eAssertionNEvent :: Iso (Expr, Expr) Expr
eAssertionNEvent =
  (Iso (\(x_aeaG, x_aeaH) -> Just ((EAssertionNEvent x_aeaG) x_aeaH)))
    (\x_aeaI ->
       case x_aeaI of
         EAssertionNEvent x_aeaG x_aeaH -> Just (x_aeaG, x_aeaH)
         _ -> Nothing)

eAssertionConstraint :: Iso (Expr, Expr) Expr
eAssertionConstraint =
  (Iso (\(x_aeaJ, x_aeaK) -> Just ((EAssertionConstraint x_aeaJ) x_aeaK)))
    (\x_aeaL ->
       case x_aeaL of
         EAssertionConstraint x_aeaJ x_aeaK -> Just (x_aeaJ, x_aeaK)
         _ -> Nothing)

eAssertionAnd :: Iso (Expr, (Expr, (Expr, Expr))) Expr
eAssertionAnd =
  (Iso
     (\(x_aeaM, (x_aeaN, (x_aeaO, x_aeaP))) ->
        Just ((((EAssertionAnd x_aeaM) x_aeaN) x_aeaO) x_aeaP)))
    (\x_aeaQ ->
       case x_aeaQ of
         EAssertionAnd x_aeaM x_aeaN x_aeaO x_aeaP ->
           Just (x_aeaM, (x_aeaN, (x_aeaO, x_aeaP)))
         _ -> Nothing)

eAssertionImplies :: Iso (Expr, (Expr, (Expr, Expr))) Expr
eAssertionImplies =
  (Iso
     (\(x_aeaR, (x_aeaS, (x_aeaT, x_aeaU))) ->
        Just ((((EAssertionImplies x_aeaR) x_aeaS) x_aeaT) x_aeaU)))
    (\x_aeaV ->
       case x_aeaV of
         EAssertionImplies x_aeaR x_aeaS x_aeaT x_aeaU ->
           Just (x_aeaR, (x_aeaS, (x_aeaT, x_aeaU)))
         _ -> Nothing)

ePartyProtocolSend ::
     Iso (Expr, (Channel, (Expr, (Label, (Expr, (VarFirst, (Expr, Expr))))))) Expr
ePartyProtocolSend =
  (Iso
     (\(x_aeaW, (x_aeaX, (x_aeaY, (x_aeaZ, (x_aeb0, (x_aeb1, (x_aeb2, x_aeb3))))))) ->
        Just
          ((((((((EPartyProtocolSend x_aeaW) x_aeaX) x_aeaY) x_aeaZ) x_aeb0)
               x_aeb1)
              x_aeb2)
             x_aeb3)))
    (\x_aeb4 ->
       case x_aeb4 of
         EPartyProtocolSend x_aeaW x_aeaX x_aeaY x_aeaZ x_aeb0 x_aeb1 x_aeb2 x_aeb3 ->
           Just
             ( x_aeaW
             , ( x_aeaX
               , (x_aeaY, (x_aeaZ, (x_aeb0, (x_aeb1, (x_aeb2, x_aeb3)))))))
         _ -> Nothing)

ePartyProtocolReceive ::
     Iso (Expr, (Channel, (Expr, (Label, (Expr, (VarFirst, (Expr, Expr))))))) Expr
ePartyProtocolReceive =
  (Iso
     (\(x_aeb5, (x_aeb6, (x_aeb7, (x_aeb8, (x_aeb9, (x_aeba, (x_aebb, x_aebc))))))) ->
        Just
          ((((((((EPartyProtocolReceive x_aeb5) x_aeb6) x_aeb7) x_aeb8) x_aeb9)
               x_aeba)
              x_aebb)
             x_aebc)))
    (\x_aebd ->
       case x_aebd of
         EPartyProtocolReceive x_aeb5 x_aeb6 x_aeb7 x_aeb8 x_aeb9 x_aeba x_aebb x_aebc ->
           Just
             ( x_aeb5
             , ( x_aeb6
               , (x_aeb7, (x_aeb8, (x_aeb9, (x_aeba, (x_aebb, x_aebc)))))))
         _ -> Nothing)

ePartyProtocolConcurrency :: Iso (Expr, (Expr, (Expr, Expr))) Expr
ePartyProtocolConcurrency =
  (Iso
     (\(x_aebe, (x_aebf, (x_aebg, x_aebh))) ->
        Just ((((EPartyProtocolConcurrency x_aebe) x_aebf) x_aebg) x_aebh)))
    (\x_aebi ->
       case x_aebi of
         EPartyProtocolConcurrency x_aebe x_aebf x_aebg x_aebh ->
           Just (x_aebe, (x_aebf, (x_aebg, x_aebh)))
         _ -> Nothing)

ePartyProtocolChoice :: Iso (Expr, (Expr, (Expr, Expr))) Expr
ePartyProtocolChoice =
  (Iso
     (\(x_aebj, (x_aebk, (x_aebl, x_aebm))) ->
        Just ((((EPartyProtocolChoice x_aebj) x_aebk) x_aebl) x_aebm)))
    (\x_aebn ->
       case x_aebn of
         EPartyProtocolChoice x_aebj x_aebk x_aebl x_aebm ->
           Just (x_aebj, (x_aebk, (x_aebl, x_aebm)))
         _ -> Nothing)

ePartyProtocolSequencing :: Iso (Expr, (Expr, (Expr, Expr))) Expr
ePartyProtocolSequencing =
  (Iso
     (\(x_aebo, (x_aebp, (x_aebq, x_aebr))) ->
        Just ((((EPartyProtocolSequencing x_aebo) x_aebp) x_aebq) x_aebr)))
    (\x_aebs ->
       case x_aebs of
         EPartyProtocolSequencing x_aebo x_aebp x_aebq x_aebr ->
           Just (x_aebo, (x_aebp, (x_aebq, x_aebr)))
         _ -> Nothing)

ePartyProtocolAssumption :: Iso (Expr, Expr) Expr
ePartyProtocolAssumption =
  (Iso (\(x_aebt, x_aebu) -> Just ((EPartyProtocolAssumption x_aebt) x_aebu)))
    (\x_aebv ->
       case x_aebv of
         EPartyProtocolAssumption x_aebt x_aebu -> Just (x_aebt, x_aebu)
         _ -> Nothing)

ePartyProtocolGuard :: Iso (Expr, Expr) Expr
ePartyProtocolGuard =
  (Iso (\(x_aebw, x_aebx) -> Just ((EPartyProtocolGuard x_aebw) x_aebx)))
    (\x_aeby ->
       case x_aeby of
         EPartyProtocolGuard x_aebw x_aebx -> Just (x_aebw, x_aebx)
         _ -> Nothing)

eEndpointProtocolSend ::
     Iso (Expr, (Channel, (Expr, (Label, (Expr, (VarFirst, (Expr, Expr))))))) Expr
eEndpointProtocolSend =
  (Iso
     (\(x_aebz, (x_aebA, (x_aebB, (x_aebC, (x_aebD, (x_aebE, (x_aebF, x_aebG))))))) ->
        Just
          ((((((((EEndpointProtocolSend x_aebz) x_aebA) x_aebB) x_aebC) x_aebD)
               x_aebE)
              x_aebF)
             x_aebG)))
    (\x_aebH ->
       case x_aebH of
         EEndpointProtocolSend x_aebz x_aebA x_aebB x_aebC x_aebD x_aebE x_aebF x_aebG ->
           Just
             ( x_aebz
             , ( x_aebA
               , (x_aebB, (x_aebC, (x_aebD, (x_aebE, (x_aebF, x_aebG)))))))
         _ -> Nothing)

eEndpointProtocolReceive ::
     Iso (Expr, (Channel, (Expr, (Label, (Expr, (VarFirst, (Expr, Expr))))))) Expr
eEndpointProtocolReceive =
  (Iso
     (\(x_aebI, (x_aebJ, (x_aebK, (x_aebL, (x_aebM, (x_aebN, (x_aebO, x_aebP))))))) ->
        Just
          ((((((((EEndpointProtocolReceive x_aebI) x_aebJ) x_aebK) x_aebL)
                x_aebM)
               x_aebN)
              x_aebO)
             x_aebP)))
    (\x_aebQ ->
       case x_aebQ of
         EEndpointProtocolReceive x_aebI x_aebJ x_aebK x_aebL x_aebM x_aebN x_aebO x_aebP ->
           Just
             ( x_aebI
             , ( x_aebJ
               , (x_aebK, (x_aebL, (x_aebM, (x_aebN, (x_aebO, x_aebP)))))))
         _ -> Nothing)

eEndpointProtocolConcurrency :: Iso (Expr, (Expr, (Expr, Expr))) Expr
eEndpointProtocolConcurrency =
  (Iso
     (\(x_aebR, (x_aebS, (x_aebT, x_aebU))) ->
        Just ((((EEndpointProtocolConcurrency x_aebR) x_aebS) x_aebT) x_aebU)))
    (\x_aebV ->
       case x_aebV of
         EEndpointProtocolConcurrency x_aebR x_aebS x_aebT x_aebU ->
           Just (x_aebR, (x_aebS, (x_aebT, x_aebU)))
         _ -> Nothing)

eEndpointProtocolChoice :: Iso (Expr, (Expr, (Expr, Expr))) Expr
eEndpointProtocolChoice =
  (Iso
     (\(x_aebW, (x_aebX, (x_aebY, x_aebZ))) ->
        Just ((((EEndpointProtocolChoice x_aebW) x_aebX) x_aebY) x_aebZ)))
    (\x_aec0 ->
       case x_aec0 of
         EEndpointProtocolChoice x_aebW x_aebX x_aebY x_aebZ ->
           Just (x_aebW, (x_aebX, (x_aebY, x_aebZ)))
         _ -> Nothing)

eEndpointProtocolSequencing :: Iso (Expr, (Expr, (Expr, Expr))) Expr
eEndpointProtocolSequencing =
  (Iso
     (\(x_aec1, (x_aec2, (x_aec3, x_aec4))) ->
        Just ((((EEndpointProtocolSequencing x_aec1) x_aec2) x_aec3) x_aec4)))
    (\x_aec5 ->
       case x_aec5 of
         EEndpointProtocolSequencing x_aec1 x_aec2 x_aec3 x_aec4 ->
           Just (x_aec1, (x_aec2, (x_aec3, x_aec4)))
         _ -> Nothing)

eEndpointProtocolAssumption :: Iso (Expr, Expr) Expr
eEndpointProtocolAssumption =
  (Iso (\(x_aec6, x_aec7) -> Just ((EEndpointProtocolAssumption x_aec6) x_aec7)))
    (\x_aec8 ->
       case x_aec8 of
         EEndpointProtocolAssumption x_aec6 x_aec7 -> Just (x_aec6, x_aec7)
         _ -> Nothing)

eEndpointProtocolGuard :: Iso (Expr, Expr) Expr
eEndpointProtocolGuard =
  (Iso (\(x_aec9, x_aeca) -> Just ((EEndpointProtocolGuard x_aec9) x_aeca)))
    (\x_aecb ->
       case x_aecb of
         EEndpointProtocolGuard x_aec9 x_aeca -> Just (x_aec9, x_aeca)
         _ -> Nothing)

eChannelProtocolTransmission ::
     Iso ( Expr
         , ( Role
           , (Expr, (Label, (Expr, (Role, (Expr, (VarFirst, (Expr, Expr))))))))) Expr
eChannelProtocolTransmission =
  (Iso
     (\(x_aecc, (x_aecd, (x_aece, (x_aecf, (x_aecg, (x_aech, (x_aeci, (x_aecj, (x_aeck, x_aecl))))))))) ->
        Just
          ((((((((((EChannelProtocolTransmission x_aecc) x_aecd) x_aece) x_aecf)
                  x_aecg)
                 x_aech)
                x_aeci)
               x_aecj)
              x_aeck)
             x_aecl)))
    (\x_aecm ->
       case x_aecm of
         EChannelProtocolTransmission x_aecc x_aecd x_aece x_aecf x_aecg x_aech x_aeci x_aecj x_aeck x_aecl ->
           Just
             ( x_aecc
             , ( x_aecd
               , ( x_aece
                 , ( x_aecf
                   , (x_aecg, (x_aech, (x_aeci, (x_aecj, (x_aeck, x_aecl)))))))))
         _ -> Nothing)

eChannelProtocolChoice :: Iso (Expr, (Expr, (Expr, Expr))) Expr
eChannelProtocolChoice =
  (Iso
     (\(x_aecn, (x_aeco, (x_aecp, x_aecq))) ->
        Just ((((EChannelProtocolChoice x_aecn) x_aeco) x_aecp) x_aecq)))
    (\x_aecr ->
       case x_aecr of
         EChannelProtocolChoice x_aecn x_aeco x_aecp x_aecq ->
           Just (x_aecn, (x_aeco, (x_aecp, x_aecq)))
         _ -> Nothing)

eChannelProtocolSequencing :: Iso (Expr, (Expr, (Expr, Expr))) Expr
eChannelProtocolSequencing =
  (Iso
     (\(x_aecs, (x_aect, (x_aecu, x_aecv))) ->
        Just ((((EChannelProtocolSequencing x_aecs) x_aect) x_aecu) x_aecv)))
    (\x_aecw ->
       case x_aecw of
         EChannelProtocolSequencing x_aecs x_aect x_aecu x_aecv ->
           Just (x_aecs, (x_aect, (x_aecu, x_aecv)))
         _ -> Nothing)

eChannelProtocolAssumption :: Iso (Expr, Expr) Expr
eChannelProtocolAssumption =
  (Iso (\(x_aecx, x_aecy) -> Just ((EChannelProtocolAssumption x_aecx) x_aecy)))
    (\x_aecz ->
       case x_aecz of
         EChannelProtocolAssumption x_aecx x_aecy -> Just (x_aecx, x_aecy)
         _ -> Nothing)

eChannelProtocolGuard :: Iso (Expr, Expr) Expr
eChannelProtocolGuard =
  (Iso (\(x_aecA, x_aecB) -> Just ((EChannelProtocolGuard x_aecA) x_aecB)))
    (\x_aecC ->
       case x_aecC of
         EChannelProtocolGuard x_aecA x_aecB -> Just (x_aecA, x_aecB)
         _ -> Nothing)

eOpLift :: Iso OpLift Expr
eOpLift =
  (Iso (\x_aecD -> Just (EOpLift x_aecD)))
    (\x_aecE ->
       case x_aecE of
         EOpLift x_aecD -> Just x_aecD
         _ -> Nothing)

eOpNullary :: Iso OpNullary Expr
eOpNullary =
  (Iso (\x_aecF -> Just (EOpNullary x_aecF)))
    (\x_aecG ->
       case x_aecG of
         EOpNullary x_aecF -> Just x_aecF
         _ -> Nothing)

eOpUnary :: Iso (OpUnary, Expr) Expr
eOpUnary =
  (Iso (\(x_aecH, x_aecI) -> Just ((EOpUnary x_aecH) x_aecI)))
    (\x_aecJ ->
       case x_aecJ of
         EOpUnary x_aecH x_aecI -> Just (x_aecH, x_aecI)
         _ -> Nothing)

eOpBinary :: Iso (Expr, (OpBinary, Expr)) Expr
eOpBinary =
  (Iso
     (\(x_aecK, (x_aecL, x_aecM)) -> Just (((EOpBinary x_aecK) x_aecL) x_aecM)))
    (\x_aecN ->
       case x_aecN of
         EOpBinary x_aecK x_aecL x_aecM -> Just (x_aecK, (x_aecL, x_aecM))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpLift)
eVarFirst :: Iso VarFirst OpLift
eVarFirst =
  (Iso (\x_aes9 -> Just (EVarFirst x_aes9)))
    (\x_aesa ->
       case x_aesa of
         EVarFirst x_aes9 -> Just x_aes9
         _ -> Nothing)

eDataStructure :: Iso DataStructure OpLift
eDataStructure =
  (Iso (\x_aesb -> Just (EDataStructure x_aesb)))
    (\x_aesc ->
       case x_aesc of
         EDataStructure x_aesb -> Just x_aesb
         _ -> Nothing)

eVarType :: Iso VarType OpLift
eVarType =
  (Iso (\x_aesd -> Just (EVarType x_aesd)))
    (\x_aese ->
       case x_aese of
         EVarType x_aesd -> Just x_aesd
         _ -> Nothing)

ePredicate :: Iso Predicate OpLift
ePredicate =
  (Iso (\x_aesf -> Just (EPredicate x_aesf)))
    (\x_aesg ->
       case x_aesg of
         EPredicate x_aesf -> Just x_aesf
         _ -> Nothing)

eRole :: Iso Role OpLift
eRole =
  (Iso (\x_aesh -> Just (ERole x_aesh)))
    (\x_aesi ->
       case x_aesi of
         ERole x_aesh -> Just x_aesh
         _ -> Nothing)

eChannel :: Iso Channel OpLift
eChannel =
  (Iso (\x_aesj -> Just (EChannel x_aesj)))
    (\x_aesk ->
       case x_aesk of
         EChannel x_aesj -> Just x_aesj
         _ -> Nothing)

eLabel :: Iso Label OpLift
eLabel =
  (Iso (\x_aesl -> Just (ELabel x_aesl)))
    (\x_aesm ->
       case x_aesm of
         ELabel x_aesl -> Just x_aesl
         _ -> Nothing)

eBool :: Iso Bool OpLift
eBool =
  (Iso (\x_aesn -> Just (EBool x_aesn)))
    (\x_aeso ->
       case x_aeso of
         EBool x_aesn -> Just x_aesn
         _ -> Nothing)

eInteger :: Iso Integer OpLift
eInteger =
  (Iso (\x_aesp -> Just (EInteger x_aesp)))
    (\x_aesq ->
       case x_aesq of
         EInteger x_aesp -> Just x_aesp
         _ -> Nothing)

-- $(defineIsomorphisms ''OpNullary)
eHeapEmp :: Iso () OpNullary
eHeapEmp =
  (Iso (\() -> Just EHeapEmp))
    (\x_aeuJ ->
       case x_aeuJ of
         EHeapEmp -> Just ()
         _ -> Nothing)

eGlobalProtocolEmp :: Iso () OpNullary
eGlobalProtocolEmp =
  (Iso (\() -> Just EGlobalProtocolEmp))
    (\x_aeuK ->
       case x_aeuK of
         EGlobalProtocolEmp -> Just ()
         _ -> Nothing)

ePartyProtocolEmp :: Iso () OpNullary
ePartyProtocolEmp =
  (Iso (\() -> Just EPartyProtocolEmp))
    (\x_aeuL ->
       case x_aeuL of
         EPartyProtocolEmp -> Just ()
         _ -> Nothing)

eEndpointProtocolEmp :: Iso () OpNullary
eEndpointProtocolEmp =
  (Iso (\() -> Just EEndpointProtocolEmp))
    (\x_aeuM ->
       case x_aeuM of
         EEndpointProtocolEmp -> Just ()
         _ -> Nothing)

eChannelProtocolEmp :: Iso () OpNullary
eChannelProtocolEmp =
  (Iso (\() -> Just EChannelProtocolEmp))
    (\x_aeuN ->
       case x_aeuN of
         EChannelProtocolEmp -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''OpUnary)
ePureBool :: Iso () OpUnary
ePureBool =
  (Iso (\() -> Just EPureBool))
    (\x_aew8 ->
       case x_aew8 of
         EPureBool -> Just ()
         _ -> Nothing)

ePureNot :: Iso () OpUnary
ePureNot =
  (Iso (\() -> Just EPureNot))
    (\x_aew9 ->
       case x_aew9 of
         EPureNot -> Just ()
         _ -> Nothing)

ePureBoolInteger :: Iso () OpUnary
ePureBoolInteger =
  (Iso (\() -> Just EPureBoolInteger))
    (\x_aewa ->
       case x_aewa of
         EPureBoolInteger -> Just ()
         _ -> Nothing)

ePurePointer :: Iso () OpUnary
ePurePointer =
  (Iso (\() -> Just EPurePointer))
    (\x_aewb ->
       case x_aewb of
         EPurePointer -> Just ()
         _ -> Nothing)

ePointerNull :: Iso () OpUnary
ePointerNull =
  (Iso (\() -> Just EPointerNull))
    (\x_aewc ->
       case x_aewc of
         EPointerNull -> Just ()
         _ -> Nothing)

ePointerNNull :: Iso () OpUnary
ePointerNNull =
  (Iso (\() -> Just EPointerNNull))
    (\x_aewd ->
       case x_aewd of
         EPointerNNull -> Just ()
         _ -> Nothing)

eIntegerNeg :: Iso () OpUnary
eIntegerNeg =
  (Iso (\() -> Just EIntegerNeg))
    (\x_aewe ->
       case x_aewe of
         EIntegerNeg -> Just ()
         _ -> Nothing)

eIntegerVarFirst :: Iso () OpUnary
eIntegerVarFirst =
  (Iso (\() -> Just EIntegerVarFirst))
    (\x_aewf ->
       case x_aewf of
         EIntegerVarFirst -> Just ()
         _ -> Nothing)

eGlobalProtocolAssumption :: Iso () OpUnary
eGlobalProtocolAssumption =
  (Iso (\() -> Just EGlobalProtocolAssumption))
    (\x_aewg ->
       case x_aewg of
         EGlobalProtocolAssumption -> Just ()
         _ -> Nothing)

eGlobalProtocolGuard :: Iso () OpUnary
eGlobalProtocolGuard =
  (Iso (\() -> Just EGlobalProtocolGuard))
    (\x_aewh ->
       case x_aewh of
         EGlobalProtocolGuard -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''OpBinary)
eHeapSeparate :: Iso () OpBinary
eHeapSeparate =
  (Iso (\() -> Just EHeapSeparate))
    (\x_aeyP ->
       case x_aeyP of
         EHeapSeparate -> Just ()
         _ -> Nothing)

eFormulaSeparate :: Iso () OpBinary
eFormulaSeparate =
  (Iso (\() -> Just EFormulaSeparate))
    (\x_aeyQ ->
       case x_aeyQ of
         EFormulaSeparate -> Just ()
         _ -> Nothing)

ePureAnd :: Iso () OpBinary
ePureAnd =
  (Iso (\() -> Just EPureAnd))
    (\x_aeyR ->
       case x_aeyR of
         EPureAnd -> Just ()
         _ -> Nothing)

ePureOr :: Iso () OpBinary
ePureOr =
  (Iso (\() -> Just EPureOr))
    (\x_aeyS ->
       case x_aeyS of
         EPureOr -> Just ()
         _ -> Nothing)

ePureVarType :: Iso () OpBinary
ePureVarType =
  (Iso (\() -> Just EPureVarType))
    (\x_aeyT ->
       case x_aeyT of
         EPureVarType -> Just ()
         _ -> Nothing)

ePureExists :: Iso () OpBinary
ePureExists =
  (Iso (\() -> Just EPureExists))
    (\x_aeyU ->
       case x_aeyU of
         EPureExists -> Just ()
         _ -> Nothing)

ePureForall :: Iso () OpBinary
ePureForall =
  (Iso (\() -> Just EPureForall))
    (\x_aeyV ->
       case x_aeyV of
         EPureForall -> Just ()
         _ -> Nothing)

ePointerEq :: Iso () OpBinary
ePointerEq =
  (Iso (\() -> Just EPointerEq))
    (\x_aeyW ->
       case x_aeyW of
         EPointerEq -> Just ()
         _ -> Nothing)

ePointerNEq :: Iso () OpBinary
ePointerNEq =
  (Iso (\() -> Just EPointerNEq))
    (\x_aeyX ->
       case x_aeyX of
         EPointerNEq -> Just ()
         _ -> Nothing)

eBoolEq :: Iso () OpBinary
eBoolEq =
  (Iso (\() -> Just EBoolEq))
    (\x_aeyY ->
       case x_aeyY of
         EBoolEq -> Just ()
         _ -> Nothing)

eBoolIntegerEq :: Iso () OpBinary
eBoolIntegerEq =
  (Iso (\() -> Just EBoolIntegerEq))
    (\x_aeyZ ->
       case x_aeyZ of
         EBoolIntegerEq -> Just ()
         _ -> Nothing)

eBoolIntegerLeq :: Iso () OpBinary
eBoolIntegerLeq =
  (Iso (\() -> Just EBoolIntegerLeq))
    (\x_aez0 ->
       case x_aez0 of
         EBoolIntegerLeq -> Just ()
         _ -> Nothing)

eIntegerMul :: Iso () OpBinary
eIntegerMul =
  (Iso (\() -> Just EIntegerMul))
    (\x_aez1 ->
       case x_aez1 of
         EIntegerMul -> Just ()
         _ -> Nothing)

eIntegerAdd :: Iso () OpBinary
eIntegerAdd =
  (Iso (\() -> Just EIntegerAdd))
    (\x_aez2 ->
       case x_aez2 of
         EIntegerAdd -> Just ()
         _ -> Nothing)

eGlobalProtocolConcurrency :: Iso () OpBinary
eGlobalProtocolConcurrency =
  (Iso (\() -> Just EGlobalProtocolConcurrency))
    (\x_aez3 ->
       case x_aez3 of
         EGlobalProtocolConcurrency -> Just ()
         _ -> Nothing)

eGlobalProtocolChoice :: Iso () OpBinary
eGlobalProtocolChoice =
  (Iso (\() -> Just EGlobalProtocolChoice))
    (\x_aez4 ->
       case x_aez4 of
         EGlobalProtocolChoice -> Just ()
         _ -> Nothing)

eGlobalProtocolSequencing :: Iso () OpBinary
eGlobalProtocolSequencing =
  (Iso (\() -> Just EGlobalProtocolSequencing))
    (\x_aez5 ->
       case x_aez5 of
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
