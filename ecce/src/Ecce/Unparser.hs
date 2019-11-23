{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows patterm match on GADT

{-
 - SECTION MODULE
 -}
module Ecce.Unparser where

{-
 - SECTION IMPORTS
 -}
import Control.Monad (join)
import Data.List (intercalate)
import Ecce.Base (extractParse)
import Ecce.Interpreter (Output, mainHaskeline)
import Ecce.Parser (AnyExpr(AnyExpr), AnyExpr, Expr(..), parseExpr)

{-
 - SECTION USER INTERFACE
 -}
type Test = (Integer, String, String)

main :: IO ()
main = mainRegular commandOutputs incommandOutput

commandOutputs :: [(String, Output)]
commandOutputs =
  [ ( "help"
    , \(_, commands, _) ->
        mapM_ putStrLn $ "Here are a list of commands:" : commands)
  ]

incommandOutput :: Output
incommandOutput =
  \(inputLine, _, _) ->
    putStrLn $ either show un $ extractParse parseExpr inputLine

{-
 - SUBSECTION UNPARSER
 -}
un :: AnyExpr -> String
un (AnyExpr e)
  -- Constructors defined in Expr
 =
  case e of
    ESymbolicPredicate pr es fd pu ->
      join $
      un (AnyExpr pr) :
      "(" : unSep "," es : ")=" : un (AnyExpr fd) : "inv" : un (AnyExpr pu) : []
    EFormulaDisjunct fs -> unSep "|" (map AnyExpr fs)
    EFormulaExists vs h p ->
      join $
      "E" :
      unSep "," (map AnyExpr vs) :
      "." : un (AnyExpr h) : "^" : un (AnyExpr p) : []
    EFormulaSeparate f1 f2 -> unSep "*" (AnyExpr f1 : AnyExpr f2 : [])
    EHeapEmp -> "emp"
    EHeapMap v1 d vs ->
      join $
      un (AnyExpr v1) : "->" : un (AnyExpr d) : unSep "," (map AnyExpr vs) : []
    EHeapPredicate p es -> join $ un (AnyExpr p) : unSep "," es : []
    EHeapSeparate h1 h2 -> unSep "," (AnyExpr h1 : AnyExpr h2 : [])
    EPureVarType v t -> join $ un (AnyExpr v) : ":" : un (AnyExpr t) : []
    EPureBool b -> un (AnyExpr b)
    EPureBoolInteger bi -> un (AnyExpr bi)
    EPureAnd p1 p2 -> unSep "^" (AnyExpr p1 : AnyExpr p2 : [])
    EPureOr p1 p2 -> unSep "|" (AnyExpr p1 : AnyExpr p2 : [])
    EPureNot p -> join $ "~" : un (AnyExpr p) : []
    EPureExists v p -> join $ "E" : un (AnyExpr v) : "." : un (AnyExpr p) : []
    EPureForall v p -> join $ "A" : un (AnyExpr v) : "." : un (AnyExpr p) : []
    EPurePointer p -> un (AnyExpr p)
    EPointerEq v1 v2 -> join $ un (AnyExpr v1) : "=" : un (AnyExpr v2) : []
    EPointerNull v -> join $ un (AnyExpr v) : "=null" : []
    EPointerNEq v1 v2 -> join $ un (AnyExpr v1) : "/=" : un (AnyExpr v2) : []
    EPointerNNull v -> join $ un (AnyExpr v) : "/=null" : []
    EBool b ->
      if b
        then "true"
        else "false"
    EBoolEq b1 b2 -> unSep "=" (AnyExpr b1 : AnyExpr b2 : [])
    EBoolIntegerEq s1 s2 -> join $ un (AnyExpr s1) : "=" : un (AnyExpr s2) : []
    EBoolIntegerLeq s1 s2 ->
      join $ un (AnyExpr s1) : "<=" : un (AnyExpr s2) : []
    EInteger i -> show i
    EIntegerVarFirst v -> un (AnyExpr v)
    EIntegerMul i1 i2 -> unSep "x" (AnyExpr i1 : AnyExpr i2 : [])
    EIntegerAdd i1 i2 -> unSep "+" (AnyExpr i1 : AnyExpr i2 : [])
    EIntegerNeg i -> join $ "-" : un (AnyExpr i) : []
    EGlobalProtocolTransmission s i r c v f ->
      join $
      un (AnyExpr s) :
      "--(" :
      un (AnyExpr i) :
      ")->" :
      un (AnyExpr r) :
      ":" :
      un (AnyExpr c) : "<" : un (AnyExpr v) : "." : un (AnyExpr f) : ">" : []
    EGlobalProtocolConcurrency g1 g2 -> unSep "*" (AnyExpr g1 : AnyExpr g2 : [])
    EGlobalProtocolChoice g1 g2 -> unSep "|" (AnyExpr g1 : AnyExpr g2 : [])
    EGlobalProtocolSequencing g1 g2 -> unSep ";" (AnyExpr g1 : AnyExpr g2 : [])
    EGlobalProtocolAssumption a ->
      join $ "Assumption(" : un (AnyExpr a) : ")" : []
    EGlobalProtocolGuard a -> join $ "Guard(" : un (AnyExpr a) : ")" : []
    EGlobalProtocolEmp -> "emp"
    EEvent p i -> join $ un (AnyExpr p) : "(" : un (AnyExpr i) : ")" : []
    EConstraintCommunicates e1 e2 ->
      join $ un (AnyExpr e1) : "<CB" : un (AnyExpr e2) : []
    EConstraintHappens e1 e2 ->
      join $ un (AnyExpr e1) : "<HB" : un (AnyExpr e2) : []
    EAssertionEvent e -> un (AnyExpr e)
    EAssertionNEvent e -> join $ "~" : un (AnyExpr e) : []
    EAssertionConstraint c -> un (AnyExpr c)
    EAssertionAnd a1 a2 -> unSep "^" (AnyExpr a1 : AnyExpr a2 : [])
    EAssertionImplies e a -> join $ un (AnyExpr e) : "==>" : un (AnyExpr a) : []
    EPartyProtocolSend c i v f ->
      join $
      un (AnyExpr c) :
      "(" : un (AnyExpr i) : ")!:" : un (AnyExpr v) : "." : un (AnyExpr f) : []
    EPartyProtocolReceive c i v f ->
      join $
      un (AnyExpr c) :
      "(" : un (AnyExpr i) : ")?:" : un (AnyExpr v) : "." : un (AnyExpr f) : []
    EPartyProtocolConcurrency p1 p2 -> unSep "*" (AnyExpr p1 : AnyExpr p2 : [])
    EPartyProtocolChoice p1 p2 -> unSep "|" (AnyExpr p1 : AnyExpr p2 : [])
    EPartyProtocolSequencing p1 p2 -> unSep ";" (AnyExpr p1 : AnyExpr p2 : [])
    EPartyProtocolAssumption a ->
      join $ "Assumption(" : un (AnyExpr a) : ")" : []
    EPartyProtocolGuard a -> join $ "Guard(" : un (AnyExpr a) : ")" : []
    EPartyProtocolEmp -> "emp"
    EEndpointProtocolSend c i v f ->
      join $
      un (AnyExpr c) :
      "(" : un (AnyExpr i) : ")!:" : un (AnyExpr v) : "." : un (AnyExpr f) : []
    EEndpointProtocolReceive c i v f ->
      join $
      un (AnyExpr c) :
      "(" : un (AnyExpr i) : ")?:" : un (AnyExpr v) : "." : un (AnyExpr f) : []
    EEndpointProtocolConcurrency e1 e2 ->
      unSep "*" (AnyExpr e1 : AnyExpr e2 : [])
    EEndpointProtocolChoice e1 e2 -> unSep "|" (AnyExpr e1 : AnyExpr e2 : [])
    EEndpointProtocolSequencing e1 e2 ->
      unSep ";" (AnyExpr e1 : AnyExpr e2 : [])
    EEndpointProtocolAssumption a ->
      join $ "Assumption(" : un (AnyExpr a) : ")" : []
    EEndpointProtocolGuard a -> join $ "Guard(" : un (AnyExpr a) : ")" : []
    EEndpointProtocolEmp -> "emp"
    EChannelProtocolTransmission s i r v f ->
      join $
      un (AnyExpr s) :
      "--(" :
      un (AnyExpr i) :
      ")->" : un (AnyExpr r) : ":" : un (AnyExpr v) : "." : un (AnyExpr f) : []
    EChannelProtocolChoice g1 g2 -> unSep "|" (AnyExpr g1 : AnyExpr g2 : [])
    EChannelProtocolSequencing g1 g2 -> unSep ";" (AnyExpr g1 : AnyExpr g2 : [])
    EChannelProtocolAssumption a ->
      join $ "Assumption(" : un (AnyExpr a) : ")" : []
    EChannelProtocolGuard a -> join $ "Guard(" : un (AnyExpr a) : ")" : []
    EChannelProtocolEmp -> "emp"
    -- Helper constructors
    EVarFirst v -> show v
    EDataStructure d -> d
    EVarType t -> t
    EPredicate p -> p
    ERole p -> p
    EChannel c -> c
    ELabel i -> show i

unSep :: String -> [AnyExpr] -> String
unSep s es = intercalate s (map un es)
