\documentclass{article}
%include polycode.fmt
\begin{document}
\begin{code}
{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows patterm match on GADT

{-
 - SECTION MODULE
 -}
module Projector where

{-
 - SECTION IMPORTS
 -}
import Parser
  ( Assertion
  , Channel
  , Constraint
  , EndpointProtocol
  , Event
  , Expr(EAssertionAnd, EAssertionConstraint, EAssertionEvent,
     EAssertionImplies, EAssertionNEvent, EChannel,
     EConstraintCommunicates, EConstraintHappens,
     EEndpointProtocolChoice, EEndpointProtocolConcurrency,
     EEndpointProtocolEmp, EEndpointProtocolReceive,
     EEndpointProtocolSend, EEndpointProtocolSequencing, EEvent,
     EGlobalProtocolAssumption, EGlobalProtocolChoice,
     EGlobalProtocolConcurrency, EGlobalProtocolEmp,
     EGlobalProtocolGuard, EGlobalProtocolSequencing,
     EGlobalProtocolSequencing, EGlobalProtocolTransmission,
     EPartyProtocolChoice, EPartyProtocolConcurrency, EPartyProtocolEmp,
     EPartyProtocolReceive, EPartyProtocolSend,
     EPartyProtocolSequencing, ERole)
  , Expr
  , GlobalProtocol
  , PartyProtocol
  , Role
  )

{-
 - SECTION Formal Definitions 4.2.1
 -}
{-
 - SUBSECTION TRANSMISSION
 -}
tr :: Expr GlobalProtocol -> [Expr GlobalProtocol]
tr g =
  case g of
    EGlobalProtocolTransmission _ _ _ _ _ _ -> [g]
    EGlobalProtocolConcurrency g1 g2 -> tr g1 ++ tr g2
    EGlobalProtocolChoice g1 g2 -> tr g1 ++ tr g2
    EGlobalProtocolSequencing g1 g2 -> tr g1 ++ tr g2
    EGlobalProtocolAssumption _ -> []
    EGlobalProtocolGuard _ -> []
    EGlobalProtocolEmp -> []

{-
 - SUBSECTION EVENT
 - Note:
 -  Defined in terms of List, instead of Set in the thesis.
 -  This is because Set requires Eq to be derived for the Expr datatype,
 -  which means that certain constructors of Expr like EHeapProtocol and
 -  ESymbolicProtocol will also need to derive Eq.
 -  However, this is not possible because both of those constructors have an
 -  argument of type [AnyExpr].
 -  Hence, in deriving Eq for both of those constructors, we assume Eq for
 -  AnyExpr, which assumes Eq for both of those constructors.
 -  This is circular reasoning.
 -}
ev :: Expr GlobalProtocol -> [Expr Event]
ev g =
  case g of
    EGlobalProtocolTransmission s i r _ _ _ -> [EEvent s i, EEvent r i]
    EGlobalProtocolConcurrency g1 g2 -> ev g1 ++ ev g2
    EGlobalProtocolChoice g1 g2 -> ev g1 ++ ev g2
    EGlobalProtocolSequencing g1 g2 -> ev g1 ++ ev g2
    EGlobalProtocolAssumption a -> evAssertion a
    EGlobalProtocolGuard a -> evAssertion a
    EGlobalProtocolEmp -> []

-- Paper defines ev on Assumptions and Guards, so,
-- (1) also define ev on Assertions
-- (2) also define ev on Constraints
evAssertion :: Expr Assertion -> [Expr Event]
evAssertion a =
  case a of
    EAssertionEvent e -> [e]
    EAssertionNEvent e -> [e]
    EAssertionConstraint c -> evConstraint c
    EAssertionAnd a1 a2 -> evAssertion a1 ++ evAssertion a2
    EAssertionImplies e a -> e : evAssertion a

evConstraint :: Expr Constraint -> [Expr Event]
evConstraint c =
  case c of
    EConstraintCommunicates e1 e2 -> [e1, e2]
    EConstraintHappens e1 e2 -> [e1, e2]

{-
 - SUBSECTION GLOBAL SPEC -> PER PARTY SPEC
projectGlobalToParty :: Expr GlobalProtocol -> Expr Role -> Expr PartyProtocol
projectGlobalToParty (EGlobalProtocolTransmission (ERole s) i (ERole r) c v f) (ERole p)
  | p == s = EPartyProtocolSend c i v f
  | p == r = EPartyProtocolReceive c i v f
  | otherwise = EPartyProtocolEmp
projectGlobalToParty (EGlobalProtocolConcurrency g1 g2) p =
  EPartyProtocolConcurrency
    (projectGlobalToParty g1 p)
    (projectGlobalToParty g2 p)
projectGlobalToParty (EGlobalProtocolChoice g1 g2) p =
  EPartyProtocolChoice (projectGlobalToParty g1 p) (projectGlobalToParty g2 p)
projectGlobalToParty (EGlobalProtocolSequencing g1 g2) p =
  EPartyProtocolSequencing
    (projectGlobalToParty g1 p)
    (projectGlobalToParty g2 p)
projectGlobalToParty EGlobalProtocolEmp _ = EPartyProtocolEmp

 -}
projectGlobalToParty :: Expr GlobalProtocol -> Expr Role -> Expr PartyProtocol
projectGlobalToParty g er@(ERole p) =
  case g of
    EGlobalProtocolTransmission (ERole s) i (ERole r) c v f
      | p == s -> EPartyProtocolSend c i v f
      | p == r -> EPartyProtocolReceive c i v f
      | otherwise -> EPartyProtocolEmp
    EGlobalProtocolConcurrency g1 g2 ->
      EPartyProtocolConcurrency
        (projectGlobalToParty g1 er)
        (projectGlobalToParty g2 er)
    EGlobalProtocolChoice g1 g2 ->
      EPartyProtocolChoice
        (projectGlobalToParty g1 er)
        (projectGlobalToParty g2 er)
    EGlobalProtocolSequencing g1 g2 ->
      EPartyProtocolSequencing
        (projectGlobalToParty g1 er)
        (projectGlobalToParty g2 er)
    EGlobalProtocolEmp -> EPartyProtocolEmp

{-
 - SUBSECTION PER PARTY SPEC -> PER ENDPOINT SPEC
 -}
-- Note:
--  - projectPartyToEndpoint (EPartyProtocolConcurrency ...) here is
--  semantically same as the specification in Figure 4.6 (b). However, here we
--  don't have the well-formedness constraint.
--  - this also means the grammar for L in Parser.hs will have an extra L*L.
--  Future work:
--      - normalize the projected endpoints, checking for well-formedness:
--      L*emp, emp*L, emp*emp are well-formed, but L*L is not.
--      - when we extend our project to allow race conditions, we will need to
--      change the definition of well-formedness, and allow L*L in the grammar
--      of L. This allows concurrent usage of the same endpoint.
projectPartyToEndpoint ::
     Expr PartyProtocol -> Expr Channel -> Expr EndpointProtocol
projectPartyToEndpoint p ec@(EChannel c) =
  case p of
    EPartyProtocolSend (EChannel c1) i v f
      | c == c1 -> EEndpointProtocolSend ec i v f
      | otherwise -> EEndpointProtocolEmp
    EPartyProtocolReceive (EChannel c1) i v f
      | c == c1 -> EEndpointProtocolReceive ec i v f
      | otherwise -> EEndpointProtocolEmp
    EPartyProtocolConcurrency g1 g2 ->
      EEndpointProtocolConcurrency
        (projectPartyToEndpoint g1 ec)
        (projectPartyToEndpoint g2 ec)
    EPartyProtocolChoice g1 g2 ->
      EEndpointProtocolChoice
        (projectPartyToEndpoint g1 ec)
        (projectPartyToEndpoint g2 ec)
    EPartyProtocolSequencing g1 g2 ->
      EEndpointProtocolSequencing
        (projectPartyToEndpoint g1 ec)
        (projectPartyToEndpoint g2 ec)
    EPartyProtocolEmp -> EEndpointProtocolEmp
\end{code}
\end{document}
