\subsection{Parser}

A parser converts well-formed strings into Abstract Data Types (ADTs).  If the
string is not well-formed, there will be a parsing error that needs to be
handled.
\par
In this section, our ADT is given by the grammars in \cite{Andreea2017thesis}.
However, we also define some base ADTs that are not explicitly mentioned in the
paper, but are needed to construct the more complex ADTs.  These base ADTs are
lifted from Haskell's base data types.

%if False
\begin{code}
{-
 - SECTION PRAGMAS
 -}
-- Pragmas of invertible-parser
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-} -- Allows `deriving` on its own line
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns
{-# LANGUAGE EmptyDataDecls #-} -- Allows datatypes without constructors
{-# LANGUAGE EmptyDataDeriving #-} -- Allows deriving for empty datatypes

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
\end{code}
%endif

%if False
\begin{code}
{-
 - SECTION TYPES
 -}
{-
 - SUBSECTION HELPERS
 -}
\end{code}
%endif

Firstly, we define data types and constructors that lift the base Haskell
types: \textit{VarFirst}, \textit{DataStructure}, \textit{VarType},
\textit{Predicate}, \textit{Role}, \textit{Channel}, and \textit{Label}.  The
constructors have the same name as the data type, but with an \textit{E}
prepended to signify that it constructs an \textit{E}xpression.  We only show
one data type definition for brevity.

\begin{code}
data VarFirst =
  EVarFirst Integer
  deriving (Show)
\end{code}

%if False
\begin{code}
data DataStructure =
  EDataStructure String
  deriving (Show)

data VarType =
  EVarType String
  deriving (Show)

data Predicate =
  EPredicate String
  deriving (Show)

data Role =
  ERole String
  deriving (Show, Eq)

data Channel =
  EChannel String
  deriving (Show, Eq)

data Label =
  ELabel Integer
  deriving (Show)
\end{code}
%endif

With the base types constructed, we define the data types as in Fig. 2.2 of
\cite{Andreea2017thesis}.  Once again, for brevity, we show only the most
interesting data types.

%if False
\begin{code}
{- Figure 2.2 -}
{- pred ::= p(root,v*) = Φ inv π -}
data SymbolicPredicate =
  ESymbolicPredicate Predicate
                     [Formula]
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
\end{code}
%endif

As a convention, if a data type named \textit{Presburger} has a binary constructor,
then all binary constructors will be moved to another data type definition,
named \textit{OpTypeBinary}.  The \textit{Presburger} data type will have all its
binary constructors replaced by this:
\begin{code}
  | EOpPresburgerBinary Presburger
                        OpPresburgerBinary
                        Presburger
\end{code}

%if False
\begin{code}
  deriving (Show)
\end{code}
%endif

The \textit{OpPresburgerBinary} data type will then have the binary operators as
nullary constructors, like \textit{EPresburgerSeparate} in this example:

\begin{code}
data OpPresburgerBinary
  = EPresburgerMul
  | EPresburgerAdd
  deriving (Show)
\end{code}

We define binary constructors in such an unconventional manner, because it
makes specifying constructor precedence in the subsequent sections easier.

%if False
\begin{code}
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
  = EEndpointProtocolSend Label
                          VarFirst
                          Formula
  | EEndpointProtocolReceive Label
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
\end{code}
%endif

%if False
\begin{code}
{-
 - SECTION LEXER
 -}
\end{code}
%endif

We take the lexer from \cite{Rendel}.

%if False
\begin{code}
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
\end{code}
%endif

%if False
\begin{code}
{-
 - SECTION SPLICE
 - TODO splice templates properly
 -}
-- $(defineIsomorphisms ''VarFirst)
\end{code}
%endif

A partial isomorphism is defined for each constructor.  It consists of two
functions: one that constructs the data type, and the other that deconstructs
the data type.  We are thus establishing an isomorphism between a constructor's
inputs and its data type.
\par
Since by convention the constructor names always start with \textit{E}, and
defining an isomorphism using Template Haskell makes the first letter
lower-case, then by convention, the isomorphism names always start with
\textit{e}.

\begin{code}
eVarFirst :: Iso Integer VarFirst
eVarFirst =
  (Iso (\x_aegM -> Just (EVarFirst x_aegM)))
    (\x_aegN ->
       case x_aegN of
         EVarFirst x_aegM -> Just x_aegM)
\end{code}

%if False
\begin{code}
-- $(defineIsomorphisms ''DataStructure)
eDataStructure :: Iso String DataStructure
eDataStructure =
  (Iso (\x_aeh9 -> Just (EDataStructure x_aeh9)))
    (\x_aeha ->
       case x_aeha of
         EDataStructure x_aeh9 -> Just x_aeh9)

-- $(defineIsomorphisms ''VarType)
eVarType :: Iso String VarType
eVarType =
  (Iso (\x_aehw -> Just (EVarType x_aehw)))
    (\x_aehx ->
       case x_aehx of
         EVarType x_aehw -> Just x_aehw)

-- $(defineIsomorphisms ''Predicate)
ePredicate :: Iso String Predicate
ePredicate =
  (Iso (\x_aehT -> Just (EPredicate x_aehT)))
    (\x_aehU ->
       case x_aehU of
         EPredicate x_aehT -> Just x_aehT)

-- $(defineIsomorphisms ''Role)
eRole :: Iso String Role
eRole =
  (Iso (\x_aeig -> Just (ERole x_aeig)))
    (\x_aeih ->
       case x_aeih of
         ERole x_aeig -> Just x_aeig)

-- $(defineIsomorphisms ''Channel)
eChannel :: Iso String Channel
eChannel =
  (Iso (\x_aeiD -> Just (EChannel x_aeiD)))
    (\x_aeiE ->
       case x_aeiE of
         EChannel x_aeiD -> Just x_aeiD)

-- $(defineIsomorphisms ''Label)
eLabel :: Iso Integer Label
eLabel =
  (Iso (\x_aej0 -> Just (ELabel x_aej0)))
    (\x_aej1 ->
       case x_aej1 of
         ELabel x_aej0 -> Just x_aej0)

-- $(defineIsomorphisms ''SymbolicPredicate)
eSymbolicPredicate ::
     Iso (Predicate, ([Formula], (FormulaDisjunct, Pure))) SymbolicPredicate
eSymbolicPredicate =
  (Iso
     (\(x_aeaI, (x_aeaJ, (x_aeaK, x_aeaL))) ->
        Just ((((ESymbolicPredicate x_aeaI) x_aeaJ) x_aeaK) x_aeaL)))
    (\x_aeaM ->
       case x_aeaM of
         ESymbolicPredicate x_aeaI x_aeaJ x_aeaK x_aeaL ->
           Just (x_aeaI, (x_aeaJ, (x_aeaK, x_aeaL))))

-- $(defineIsomorphisms ''FormulaDisjunct)
eFormulaDisjunct :: Iso [Formula] FormulaDisjunct
eFormulaDisjunct =
  (Iso (\x_aebJ -> Just (EFormulaDisjunct x_aebJ)))
    (\x_aebK ->
       case x_aebK of
         EFormulaDisjunct x_aebJ -> Just x_aebJ)

-- $(defineIsomorphisms ''Formula)
eFormulaExists :: Iso ([VarFirst], (Heap, Pure)) Formula
eFormulaExists =
  (Iso
     (\(x_aebk, (x_aebl, x_aebm)) ->
        Just (((EFormulaExists x_aebk) x_aebl) x_aebm)))
    (\x_aebn ->
       case x_aebn of
         EFormulaExists x_aebk x_aebl x_aebm -> Just (x_aebk, (x_aebl, x_aebm))
         _ -> Nothing)

eOpFormulaBinary :: Iso (Formula, (OpFormulaBinary, Formula)) Formula
eOpFormulaBinary =
  (Iso
     (\(x_aebo, (x_aebp, x_aebq)) ->
        Just (((EOpFormulaBinary x_aebo) x_aebp) x_aebq)))
    (\x_aebr ->
       case x_aebr of
         EOpFormulaBinary x_aebo x_aebp x_aebq ->
           Just (x_aebo, (x_aebp, x_aebq))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpFormulaBinary)
eFormulaSeparate :: Iso () OpFormulaBinary
eFormulaSeparate =
  (Iso (\() -> Just EFormulaSeparate))
    (\x_aecj ->
       case x_aecj of
         EFormulaSeparate -> Just ())

-- $(defineIsomorphisms ''Heap)
eHeapEmp :: Iso () Heap
eHeapEmp =
  (Iso (\() -> Just EHeapEmp))
    (\x_aecF ->
       case x_aecF of
         EHeapEmp -> Just ()
         _ -> Nothing)

eHeapMap :: Iso (VarFirst, (DataStructure, [VarFirst])) Heap
eHeapMap =
  (Iso (\(x_aecG, (x_aecH, x_aecI)) -> Just (((EHeapMap x_aecG) x_aecH) x_aecI)))
    (\x_aecJ ->
       case x_aecJ of
         EHeapMap x_aecG x_aecH x_aecI -> Just (x_aecG, (x_aecH, x_aecI))
         _ -> Nothing)

eHeapPredicate :: Iso (Predicate, [Formula]) Heap
eHeapPredicate =
  (Iso (\(x_aecK, x_aecL) -> Just ((EHeapPredicate x_aecK) x_aecL)))
    (\x_aecM ->
       case x_aecM of
         EHeapPredicate x_aecK x_aecL -> Just (x_aecK, x_aecL)
         _ -> Nothing)

eOpHeapBinary :: Iso (Heap, (OpHeapBinary, Heap)) Heap
eOpHeapBinary =
  (Iso
     (\(x_aecN, (x_aecO, x_aecP)) ->
        Just (((EOpHeapBinary x_aecN) x_aecO) x_aecP)))
    (\x_aecQ ->
       case x_aecQ of
         EOpHeapBinary x_aecN x_aecO x_aecP -> Just (x_aecN, (x_aecO, x_aecP))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpHeapBinary)
eHeapSeparate :: Iso () OpHeapBinary
eHeapSeparate =
  (Iso (\() -> Just EHeapSeparate))
    (\x_aeeg ->
       case x_aeeg of
         EHeapSeparate -> Just ())

-- $(defineIsomorphisms ''Pure)
ePureVarType :: Iso (VarFirst, VarType) Pure
ePureVarType =
  (Iso (\(x_aeeC, x_aeeD) -> Just ((EPureVarType x_aeeC) x_aeeD)))
    (\x_aeeE ->
       case x_aeeE of
         EPureVarType x_aeeC x_aeeD -> Just (x_aeeC, x_aeeD)
         _ -> Nothing)

ePureBoole :: Iso Boole Pure
ePureBoole =
  (Iso (\x_aeeF -> Just (EPureBoole x_aeeF)))
    (\x_aeeG ->
       case x_aeeG of
         EPureBoole x_aeeF -> Just x_aeeF
         _ -> Nothing)

ePureBoolePresburger :: Iso BoolePresburger Pure
ePureBoolePresburger =
  (Iso (\x_aeeH -> Just (EPureBoolePresburger x_aeeH)))
    (\x_aeeI ->
       case x_aeeI of
         EPureBoolePresburger x_aeeH -> Just x_aeeH
         _ -> Nothing)

ePureNot :: Iso Pure Pure
ePureNot =
  (Iso (\x_aeeJ -> Just (EPureNot x_aeeJ)))
    (\x_aeeK ->
       case x_aeeK of
         EPureNot x_aeeJ -> Just x_aeeJ
         _ -> Nothing)

ePureExists :: Iso (VarFirst, Pure) Pure
ePureExists =
  (Iso (\(x_aeeL, x_aeeM) -> Just ((EPureExists x_aeeL) x_aeeM)))
    (\x_aeeN ->
       case x_aeeN of
         EPureExists x_aeeL x_aeeM -> Just (x_aeeL, x_aeeM)
         _ -> Nothing)

ePureForall :: Iso (VarFirst, Pure) Pure
ePureForall =
  (Iso (\(x_aeeO, x_aeeP) -> Just ((EPureForall x_aeeO) x_aeeP)))
    (\x_aeeQ ->
       case x_aeeQ of
         EPureForall x_aeeO x_aeeP -> Just (x_aeeO, x_aeeP)
         _ -> Nothing)

ePurePointer :: Iso Pointer Pure
ePurePointer =
  (Iso (\x_aeeR -> Just (EPurePointer x_aeeR)))
    (\x_aeeS ->
       case x_aeeS of
         EPurePointer x_aeeR -> Just x_aeeR
         _ -> Nothing)

eOpPureBinary :: Iso (Pure, (OpPureBinary, Pure)) Pure
eOpPureBinary =
  (Iso
     (\(x_aeeT, (x_aeeU, x_aeeV)) ->
        Just (((EOpPureBinary x_aeeT) x_aeeU) x_aeeV)))
    (\x_aeeW ->
       case x_aeeW of
         EOpPureBinary x_aeeT x_aeeU x_aeeV -> Just (x_aeeT, (x_aeeU, x_aeeV))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpPureBinary)
ePureAnd :: Iso () OpPureBinary
ePureAnd =
  (Iso (\() -> Just EPureAnd))
    (\x_aehk ->
       case x_aehk of
         EPureAnd -> Just ()
         _ -> Nothing)

ePureOr :: Iso () OpPureBinary
ePureOr =
  (Iso (\() -> Just EPureOr))
    (\x_aehl ->
       case x_aehl of
         EPureOr -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''Pointer)
ePointerEq :: Iso (VarFirst, VarFirst) Pointer
ePointerEq =
  (Iso (\(x_aehX, x_aehY) -> Just ((EPointerEq x_aehX) x_aehY)))
    (\x_aehZ ->
       case x_aehZ of
         EPointerEq x_aehX x_aehY -> Just (x_aehX, x_aehY)
         _ -> Nothing)

ePointerNull :: Iso VarFirst Pointer
ePointerNull =
  (Iso (\x_aei0 -> Just (EPointerNull x_aei0)))
    (\x_aei1 ->
       case x_aei1 of
         EPointerNull x_aei0 -> Just x_aei0
         _ -> Nothing)

ePointerNEq :: Iso (VarFirst, VarFirst) Pointer
ePointerNEq =
  (Iso (\(x_aei2, x_aei3) -> Just ((EPointerNEq x_aei2) x_aei3)))
    (\x_aei4 ->
       case x_aei4 of
         EPointerNEq x_aei2 x_aei3 -> Just (x_aei2, x_aei3)
         _ -> Nothing)

ePointerNNull :: Iso VarFirst Pointer
ePointerNNull =
  (Iso (\x_aei5 -> Just (EPointerNNull x_aei5)))
    (\x_aei6 ->
       case x_aei6 of
         EPointerNNull x_aei5 -> Just x_aei5
         _ -> Nothing)

-- $(defineIsomorphisms ''Boole)
eBoole :: Iso Bool Boole
eBoole =
  (Iso (\x_aejk -> Just (EBoole x_aejk)))
    (\x_aejl ->
       case x_aejl of
         EBoole x_aejk -> Just x_aejk
         _ -> Nothing)

eOpBooleBinary :: Iso (Boole, (OpBooleBinary, Boole)) Boole
eOpBooleBinary =
  (Iso
     (\(x_aejm, (x_aejn, x_aejo)) ->
        Just (((EOpBooleBinary x_aejm) x_aejn) x_aejo)))
    (\x_aejp ->
       case x_aejp of
         EOpBooleBinary x_aejm x_aejn x_aejo -> Just (x_aejm, (x_aejn, x_aejo))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpBooleBinary)
eBooleEq :: Iso () OpBooleBinary
eBooleEq =
  (Iso (\() -> Just EBooleEq))
    (\x_aek9 ->
       case x_aek9 of
         EBooleEq -> Just ())

-- $(defineIsomorphisms ''BoolePresburger)
eBoolePresburgerEq :: Iso (Presburger, Presburger) BoolePresburger
eBoolePresburgerEq =
  (Iso (\(x_aekv, x_aekw) -> Just ((EBoolePresburgerEq x_aekv) x_aekw)))
    (\x_aekx ->
       case x_aekx of
         EBoolePresburgerEq x_aekv x_aekw -> Just (x_aekv, x_aekw)
         _ -> Nothing)

eBoolePresburgerLeq :: Iso (Presburger, Presburger) BoolePresburger
eBoolePresburgerLeq =
  (Iso (\(x_aeky, x_aekz) -> Just ((EBoolePresburgerLeq x_aeky) x_aekz)))
    (\x_aekA ->
       case x_aekA of
         EBoolePresburgerLeq x_aeky x_aekz -> Just (x_aeky, x_aekz)
         _ -> Nothing)

-- $(defineIsomorphisms ''Presburger)
ePresburger :: Iso Integer Presburger
ePresburger =
  (Iso (\x_aelk -> Just (EPresburger x_aelk)))
    (\x_aell ->
       case x_aell of
         EPresburger x_aelk -> Just x_aelk
         _ -> Nothing)

ePresburgerVarFirst :: Iso VarFirst Presburger
ePresburgerVarFirst =
  (Iso (\x_aelm -> Just (EPresburgerVarFirst x_aelm)))
    (\x_aeln ->
       case x_aeln of
         EPresburgerVarFirst x_aelm -> Just x_aelm
         _ -> Nothing)

ePresburgerNeg :: Iso Presburger Presburger
ePresburgerNeg =
  (Iso (\x_aelo -> Just (EPresburgerNeg x_aelo)))
    (\x_aelp ->
       case x_aelp of
         EPresburgerNeg x_aelo -> Just x_aelo
         _ -> Nothing)

eOpPresburgerBinary ::
     Iso (Presburger, (OpPresburgerBinary, Presburger)) Presburger
eOpPresburgerBinary =
  (Iso
     (\(x_aelq, (x_aelr, x_aels)) ->
        Just (((EOpPresburgerBinary x_aelq) x_aelr) x_aels)))
    (\x_aelt ->
       case x_aelt of
         EOpPresburgerBinary x_aelq x_aelr x_aels ->
           Just (x_aelq, (x_aelr, x_aels))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpPresburgerBinary)
ePresburgerMul :: Iso () OpPresburgerBinary
ePresburgerMul =
  (Iso (\() -> Just EPresburgerMul))
    (\x_aemH ->
       case x_aemH of
         EPresburgerMul -> Just ()
         _ -> Nothing)

ePresburgerAdd :: Iso () OpPresburgerBinary
ePresburgerAdd =
  (Iso (\() -> Just EPresburgerAdd))
    (\x_aemI ->
       case x_aemI of
         EPresburgerAdd -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''GlobalProtocol)
eGlobalProtocolTransmission ::
     Iso (Role, (Label, (Role, (Channel, (VarFirst, Formula))))) GlobalProtocol
eGlobalProtocolTransmission =
  (Iso
     (\(x_aenk, (x_aenl, (x_aenm, (x_aenn, (x_aeno, x_aenp))))) ->
        Just
          ((((((EGlobalProtocolTransmission x_aenk) x_aenl) x_aenm) x_aenn)
              x_aeno)
             x_aenp)))
    (\x_aenq ->
       case x_aenq of
         EGlobalProtocolTransmission x_aenk x_aenl x_aenm x_aenn x_aeno x_aenp ->
           Just (x_aenk, (x_aenl, (x_aenm, (x_aenn, (x_aeno, x_aenp)))))
         _ -> Nothing)

eGlobalProtocolAssumption :: Iso Assertion GlobalProtocol
eGlobalProtocolAssumption =
  (Iso (\x_aenr -> Just (EGlobalProtocolAssumption x_aenr)))
    (\x_aens ->
       case x_aens of
         EGlobalProtocolAssumption x_aenr -> Just x_aenr
         _ -> Nothing)

eGlobalProtocolGuard :: Iso Assertion GlobalProtocol
eGlobalProtocolGuard =
  (Iso (\x_aent -> Just (EGlobalProtocolGuard x_aent)))
    (\x_aenu ->
       case x_aenu of
         EGlobalProtocolGuard x_aent -> Just x_aent
         _ -> Nothing)

eGlobalProtocolEmp :: Iso () GlobalProtocol
eGlobalProtocolEmp =
  (Iso (\() -> Just EGlobalProtocolEmp))
    (\x_aenv ->
       case x_aenv of
         EGlobalProtocolEmp -> Just ()
         _ -> Nothing)

eOpGlobalProtocolBinary ::
     Iso (GlobalProtocol, (OpGlobalProtocolBinary, GlobalProtocol)) GlobalProtocol
eOpGlobalProtocolBinary =
  (Iso
     (\(x_aenw, (x_aenx, x_aeny)) ->
        Just (((EOpGlobalProtocolBinary x_aenw) x_aenx) x_aeny)))
    (\x_aenz ->
       case x_aenz of
         EOpGlobalProtocolBinary x_aenw x_aenx x_aeny ->
           Just (x_aenw, (x_aenx, x_aeny))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpGlobalProtocolBinary)
eGlobalProtocolConcurrency :: Iso () OpGlobalProtocolBinary
eGlobalProtocolConcurrency =
  (Iso (\() -> Just EGlobalProtocolConcurrency))
    (\x_aepm ->
       case x_aepm of
         EGlobalProtocolConcurrency -> Just ()
         _ -> Nothing)

eGlobalProtocolChoice :: Iso () OpGlobalProtocolBinary
eGlobalProtocolChoice =
  (Iso (\() -> Just EGlobalProtocolChoice))
    (\x_aepn ->
       case x_aepn of
         EGlobalProtocolChoice -> Just ()
         _ -> Nothing)

eGlobalProtocolSequencing :: Iso () OpGlobalProtocolBinary
eGlobalProtocolSequencing =
  (Iso (\() -> Just EGlobalProtocolSequencing))
    (\x_aepo ->
       case x_aepo of
         EGlobalProtocolSequencing -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''Event)
eEvent :: Iso (Role, Label) Event
eEvent =
  (Iso (\(x_aeqf, x_aeqg) -> Just ((EEvent x_aeqf) x_aeqg)))
    (\x_aeqh ->
       case x_aeqh of
         EEvent x_aeqf x_aeqg -> Just (x_aeqf, x_aeqg))

-- $(defineIsomorphisms ''Constraint)
eConstraintCommunicates :: Iso (Event, Event) Constraint
eConstraintCommunicates =
  (Iso (\(x_aeqH, x_aeqI) -> Just ((EConstraintCommunicates x_aeqH) x_aeqI)))
    (\x_aeqJ ->
       case x_aeqJ of
         EConstraintCommunicates x_aeqH x_aeqI -> Just (x_aeqH, x_aeqI)
         _ -> Nothing)

eConstraintHappens :: Iso (Event, Event) Constraint
eConstraintHappens =
  (Iso (\(x_aeqK, x_aeqL) -> Just ((EConstraintHappens x_aeqK) x_aeqL)))
    (\x_aeqM ->
       case x_aeqM of
         EConstraintHappens x_aeqK x_aeqL -> Just (x_aeqK, x_aeqL)
         _ -> Nothing)

-- $(defineIsomorphisms ''Assertion)
eAssertionEvent :: Iso Event Assertion
eAssertionEvent =
  (Iso (\x_aerw -> Just (EAssertionEvent x_aerw)))
    (\x_aerx ->
       case x_aerx of
         EAssertionEvent x_aerw -> Just x_aerw
         _ -> Nothing)

eAssertionNEvent :: Iso Event Assertion
eAssertionNEvent =
  (Iso (\x_aery -> Just (EAssertionNEvent x_aery)))
    (\x_aerz ->
       case x_aerz of
         EAssertionNEvent x_aery -> Just x_aery
         _ -> Nothing)

eAssertionConstraint :: Iso Constraint Assertion
eAssertionConstraint =
  (Iso (\x_aerA -> Just (EAssertionConstraint x_aerA)))
    (\x_aerB ->
       case x_aerB of
         EAssertionConstraint x_aerA -> Just x_aerA
         _ -> Nothing)

eAssertionImplies :: Iso (Event, Assertion) Assertion
eAssertionImplies =
  (Iso (\(x_aerC, x_aerD) -> Just ((EAssertionImplies x_aerC) x_aerD)))
    (\x_aerE ->
       case x_aerE of
         EAssertionImplies x_aerC x_aerD -> Just (x_aerC, x_aerD)
         _ -> Nothing)

eOpAssertionBinary :: Iso (Assertion, (OpAssertionBinary, Assertion)) Assertion
eOpAssertionBinary =
  (Iso
     (\(x_aerF, (x_aerG, x_aerH)) ->
        Just (((EOpAssertionBinary x_aerF) x_aerG) x_aerH)))
    (\x_aerI ->
       case x_aerI of
         EOpAssertionBinary x_aerF x_aerG x_aerH ->
           Just (x_aerF, (x_aerG, x_aerH))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpAssertionBinary)
eAssertionAnd :: Iso () OpAssertionBinary
eAssertionAnd =
  (Iso (\() -> Just EAssertionAnd))
    (\x_aetf ->
       case x_aetf of
         EAssertionAnd -> Just ())

-- $(defineIsomorphisms ''PartyProtocol)
ePartyProtocolSend :: Iso (Channel, (Label, (VarFirst, Formula))) PartyProtocol
ePartyProtocolSend =
  (Iso
     (\(x_aetB, (x_aetC, (x_aetD, x_aetE))) ->
        Just ((((EPartyProtocolSend x_aetB) x_aetC) x_aetD) x_aetE)))
    (\x_aetF ->
       case x_aetF of
         EPartyProtocolSend x_aetB x_aetC x_aetD x_aetE ->
           Just (x_aetB, (x_aetC, (x_aetD, x_aetE)))
         _ -> Nothing)

ePartyProtocolReceive ::
     Iso (Channel, (Label, (VarFirst, Formula))) PartyProtocol
ePartyProtocolReceive =
  (Iso
     (\(x_aetG, (x_aetH, (x_aetI, x_aetJ))) ->
        Just ((((EPartyProtocolReceive x_aetG) x_aetH) x_aetI) x_aetJ)))
    (\x_aetK ->
       case x_aetK of
         EPartyProtocolReceive x_aetG x_aetH x_aetI x_aetJ ->
           Just (x_aetG, (x_aetH, (x_aetI, x_aetJ)))
         _ -> Nothing)

ePartyProtocolAssumption :: Iso Assertion PartyProtocol
ePartyProtocolAssumption =
  (Iso (\x_aetL -> Just (EPartyProtocolAssumption x_aetL)))
    (\x_aetM ->
       case x_aetM of
         EPartyProtocolAssumption x_aetL -> Just x_aetL
         _ -> Nothing)

ePartyProtocolGuard :: Iso Assertion PartyProtocol
ePartyProtocolGuard =
  (Iso (\x_aetN -> Just (EPartyProtocolGuard x_aetN)))
    (\x_aetO ->
       case x_aetO of
         EPartyProtocolGuard x_aetN -> Just x_aetN
         _ -> Nothing)

ePartyProtocolEmp :: Iso () PartyProtocol
ePartyProtocolEmp =
  (Iso (\() -> Just EPartyProtocolEmp))
    (\x_aetP ->
       case x_aetP of
         EPartyProtocolEmp -> Just ()
         _ -> Nothing)

eOpPartyProtocolBinary ::
     Iso (PartyProtocol, (OpPartyProtocolBinary, PartyProtocol)) PartyProtocol
eOpPartyProtocolBinary =
  (Iso
     (\(x_aetQ, (x_aetR, x_aetS)) ->
        Just (((EOpPartyProtocolBinary x_aetQ) x_aetR) x_aetS)))
    (\x_aetT ->
       case x_aetT of
         EOpPartyProtocolBinary x_aetQ x_aetR x_aetS ->
           Just (x_aetQ, (x_aetR, x_aetS))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpPartyProtocolBinary)
ePartyProtocolConcurrency :: Iso () OpPartyProtocolBinary
ePartyProtocolConcurrency =
  (Iso (\() -> Just EPartyProtocolConcurrency))
    (\x_aevZ ->
       case x_aevZ of
         EPartyProtocolConcurrency -> Just ()
         _ -> Nothing)

ePartyProtocolChoice :: Iso () OpPartyProtocolBinary
ePartyProtocolChoice =
  (Iso (\() -> Just EPartyProtocolChoice))
    (\x_aew0 ->
       case x_aew0 of
         EPartyProtocolChoice -> Just ()
         _ -> Nothing)

ePartyProtocolSequencing :: Iso () OpPartyProtocolBinary
ePartyProtocolSequencing =
  (Iso (\() -> Just EPartyProtocolSequencing))
    (\x_aew1 ->
       case x_aew1 of
         EPartyProtocolSequencing -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''EndpointProtocol)
eEndpointProtocolSend :: Iso (Label, (VarFirst, Formula)) EndpointProtocol
eEndpointProtocolSend =
  (Iso
     (\(x_aewT, (x_aewU, x_aewV)) ->
        Just (((EEndpointProtocolSend x_aewT) x_aewU) x_aewV)))
    (\x_aewW ->
       case x_aewW of
         EEndpointProtocolSend x_aewT x_aewU x_aewV ->
           Just (x_aewT, (x_aewU, x_aewV))
         _ -> Nothing)

eEndpointProtocolReceive :: Iso (Label, (VarFirst, Formula)) EndpointProtocol
eEndpointProtocolReceive =
  (Iso
     (\(x_aewY, (x_aewZ, x_aex0)) ->
        Just (((EEndpointProtocolReceive x_aewY) x_aewZ) x_aex0)))
    (\x_aex1 ->
       case x_aex1 of
         EEndpointProtocolReceive x_aewY x_aewZ x_aex0 ->
           Just (x_aewY, (x_aewZ, x_aex0))
         _ -> Nothing)

eEndpointProtocolAssumption :: Iso Assertion EndpointProtocol
eEndpointProtocolAssumption =
  (Iso (\x_aex2 -> Just (EEndpointProtocolAssumption x_aex2)))
    (\x_aex3 ->
       case x_aex3 of
         EEndpointProtocolAssumption x_aex2 -> Just x_aex2
         _ -> Nothing)

eEndpointProtocolGuard :: Iso Assertion EndpointProtocol
eEndpointProtocolGuard =
  (Iso (\x_aex4 -> Just (EEndpointProtocolGuard x_aex4)))
    (\x_aex5 ->
       case x_aex5 of
         EEndpointProtocolGuard x_aex4 -> Just x_aex4
         _ -> Nothing)

eEndpointProtocolEmp :: Iso () EndpointProtocol
eEndpointProtocolEmp =
  (Iso (\() -> Just EEndpointProtocolEmp))
    (\x_aex6 ->
       case x_aex6 of
         EEndpointProtocolEmp -> Just ()
         _ -> Nothing)

eOpEndpointProtocolBinary ::
     Iso (EndpointProtocol, (OpEndpointProtocolBinary, EndpointProtocol)) EndpointProtocol
eOpEndpointProtocolBinary =
  (Iso
     (\(x_aex7, (x_aex8, x_aex9)) ->
        Just (((EOpEndpointProtocolBinary x_aex7) x_aex8) x_aex9)))
    (\x_aexa ->
       case x_aexa of
         EOpEndpointProtocolBinary x_aex7 x_aex8 x_aex9 ->
           Just (x_aex7, (x_aex8, x_aex9))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpEndpointProtocolBinary)
eEndpointProtocolConcurrency :: Iso () OpEndpointProtocolBinary
eEndpointProtocolConcurrency =
  (Iso (\() -> Just EEndpointProtocolConcurrency))
    (\x_aezg ->
       case x_aezg of
         EEndpointProtocolConcurrency -> Just ()
         _ -> Nothing)

eEndpointProtocolChoice :: Iso () OpEndpointProtocolBinary
eEndpointProtocolChoice =
  (Iso (\() -> Just EEndpointProtocolChoice))
    (\x_aezh ->
       case x_aezh of
         EEndpointProtocolChoice -> Just ()
         _ -> Nothing)

eEndpointProtocolSequencing :: Iso () OpEndpointProtocolBinary
eEndpointProtocolSequencing =
  (Iso (\() -> Just EEndpointProtocolSequencing))
    (\x_aezi ->
       case x_aezi of
         EEndpointProtocolSequencing -> Just ()
         _ -> Nothing)

-- $(defineIsomorphisms ''ChannelProtocol)
eChannelProtocolTransmission ::
     Iso (Role, (Label, (Role, (VarFirst, Formula)))) ChannelProtocol
eChannelProtocolTransmission =
  (Iso
     (\(x_aeA9, (x_aeAa, (x_aeAb, (x_aeAc, x_aeAd)))) ->
        Just
          (((((EChannelProtocolTransmission x_aeA9) x_aeAa) x_aeAb) x_aeAc)
             x_aeAd)))
    (\x_aeAe ->
       case x_aeAe of
         EChannelProtocolTransmission x_aeA9 x_aeAa x_aeAb x_aeAc x_aeAd ->
           Just (x_aeA9, (x_aeAa, (x_aeAb, (x_aeAc, x_aeAd))))
         _ -> Nothing)

eChannelProtocolAssumption :: Iso Assertion ChannelProtocol
eChannelProtocolAssumption =
  (Iso (\x_aeAf -> Just (EChannelProtocolAssumption x_aeAf)))
    (\x_aeAg ->
       case x_aeAg of
         EChannelProtocolAssumption x_aeAf -> Just x_aeAf
         _ -> Nothing)

eChannelProtocolGuard :: Iso Assertion ChannelProtocol
eChannelProtocolGuard =
  (Iso (\x_aeAh -> Just (EChannelProtocolGuard x_aeAh)))
    (\x_aeAi ->
       case x_aeAi of
         EChannelProtocolGuard x_aeAh -> Just x_aeAh
         _ -> Nothing)

eChannelProtocolEmp :: Iso () ChannelProtocol
eChannelProtocolEmp =
  (Iso (\() -> Just EChannelProtocolEmp))
    (\x_aeAj ->
       case x_aeAj of
         EChannelProtocolEmp -> Just ()
         _ -> Nothing)

eOpChannelProtocolBinary ::
     Iso (ChannelProtocol, (OpChannelProtocolBinary, ChannelProtocol)) ChannelProtocol
eOpChannelProtocolBinary =
  (Iso
     (\(x_aeAk, (x_aeAl, x_aeAm)) ->
        Just (((EOpChannelProtocolBinary x_aeAk) x_aeAl) x_aeAm)))
    (\x_aeAn ->
       case x_aeAn of
         EOpChannelProtocolBinary x_aeAk x_aeAl x_aeAm ->
           Just (x_aeAk, (x_aeAl, x_aeAm))
         _ -> Nothing)

-- $(defineIsomorphisms ''OpChannelProtocolBinary)
eChannelProtocolChoice :: Iso () OpChannelProtocolBinary
eChannelProtocolChoice =
  (Iso (\() -> Just EChannelProtocolChoice))
    (\x_aeC6 ->
       case x_aeC6 of
         EChannelProtocolChoice -> Just ()
         _ -> Nothing)

eChannelProtocolSequencing :: Iso () OpChannelProtocolBinary
eChannelProtocolSequencing =
  (Iso (\() -> Just EChannelProtocolSequencing))
    (\x_aeC7 ->
       case x_aeC7 of
         EChannelProtocolSequencing -> Just ()
         _ -> Nothing)
\end{code}
%endif

%if False
\begin{code}
{-
 - SECTION PARSER
 -}
{-
 - SUBSECTION HELPERS
 -}
\end{code}
%endif

Parsers are applications of isomorphisms on the base type.  As an example, the
\textit{parseVarFirst} parser first lexes an \textit{Integer}, then passes the
result to the \textit{eVarFirst} constructor.  Similar parsers of other base
types are defined, but not shown.

\begin{code}
parseVarFirst :: Syntax delta => delta VarFirst
parseVarFirst = eVarFirst <$> integer
\end{code}

%if False
\begin{code}
parseDataStructure :: Syntax delta => delta DataStructure
parseDataStructure = eDataStructure <$> many token

parseVarType :: Syntax delta => delta VarType
parseVarType = eVarType <$> many token

parsePredicate :: Syntax delta => delta Predicate
parsePredicate = ePredicate <$> many token

parseRole :: Syntax delta => delta Role
parseRole = eRole <$> many token

parseChannel :: Syntax delta => delta Channel
parseChannel = eChannel <$> many token

parseLabel :: Syntax delta => delta Label
parseLabel = eLabel <$> integer
\end{code}
%endif

%if False
\begin{code}
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
    exp 0 = eFormulaDisjunct <$> parseFormula `sepBy` text "|"

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
      eFormulaExists <$>
      (text "E" *> parseVarFirst `sepBy` text "," <*> text "." *> parseHeap <*>
       text "^" *>
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
      text "E" *> (ePureExists <$> (parseVarFirst <*> parsePure)) <|>
      text "A" *> (ePureForall <$> (parseVarFirst <*> parsePure)) <|>
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
\end{code}
%endif

%if False
\begin{code}
{-
 - SUBSECTION s
 -}
\end{code}
%endif

A parser of more complex expression needs to be handled in multiple stages.  We
again look at the example of \textit{Presburger} data type.
\par
Firstly, a parser that ignores leading and trailing spaces for binary operators
is defined, \textit{.opPresburgerBinary}.  The \textit{<||>} operator has an
implied ordering, to first try the left parser, then if the left fails, the
right parser is tried.

\begin{code}
opPresburgerBinary :: Syntax delta => delta OpPresburgerBinary
opPresburgerBinary =
  between
    optSpace
    optSpace
    (ePresburgerMul <$> text "*" <|> ePresburgerAdd <$> text "+")
\end{code}

However, the ordering implied by \textit{<||>} does not matter, because we
define \textit{prioPresburgerBinary}, a mapping from constructors to
\textit{Integer}s.  Here, we have the convention that operators mapped to a
higher integer are parsed first.  For example, since we want multiplications to
have a higher precedence than additions, we want additions to be parsed before
multiplications, so that multiplication expressions are embedded inside
addition expressions.  So \textit{EPresburgerAdd} will have a priority of
\textit{2}, higher than \textit{EpresburgerMul}'s priority of 1.

\begin{code}
prioPresburgerBinary :: OpPresburgerBinary -> Integer
prioPresburgerBinary EPresburgerMul = 1
prioPresburgerBinary EPresburgerAdd = 2
\end{code}

With those two tools, we can now define the actual \textit{Presburger} parser.
Our parser is a recursive-descent parser, so have have a top-level grammar of a
binary operator of highest priority, then one or more lower-level grammars,
each with a binary operator with descending priority, and finally a base
grammar which contains all non-binary constructors, as well as the recursive
parser wrapped in parantheses.

\begin{code}
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
\end{code}

\textit{opPrioPresburgerBinary} is defined using \textit{prioPresburgerBinary}.
It returns the binary operator that matches the input priority.

\begin{code}
    opPrioPresburgerBinary n =
      eOpPresburgerBinary .
      subset (\(_, (op, _)) -> prioPresburgerBinary op == n)
\end{code}

%if False
\begin{code}
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
parseAssertion = exp 1
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
      (parens parseLabel <*> text "!:" *> parseVarFirst <*> text "." *>
       parseFormula) <|>
      eEndpointProtocolReceive <$>
      (parens parseLabel <*> text "?:" *> parseVarFirst <*> text "." *>
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
\end{code}
%endif
