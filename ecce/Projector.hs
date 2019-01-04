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
tr t@(EGlobalProtocolTransmission _ _ _ _ _ _) = [t]
tr (EGlobalProtocolConcurrency g1 g2) = tr g1 ++ tr g2
tr (EGlobalProtocolChoice g1 g2) = tr g1 ++ tr g2
tr (EGlobalProtocolSequencing g1 g2) = tr g1 ++ tr g2
tr (EGlobalProtocolAssumption _) = []
tr (EGlobalProtocolGuard _) = []
tr EGlobalProtocolEmp = []

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
ev (EGlobalProtocolTransmission s i r _ _ _) = [EEvent s i, EEvent r i]
ev (EGlobalProtocolConcurrency g1 g2) = ev g1 ++ ev g2
ev (EGlobalProtocolChoice g1 g2) = ev g1 ++ ev g2
ev (EGlobalProtocolSequencing g1 g2) = ev g1 ++ ev g2
ev (EGlobalProtocolAssumption a) = evAssertion a
ev (EGlobalProtocolGuard a) = evAssertion a
ev EGlobalProtocolEmp = []

-- Paper defines ev on Assumptions and Guards, so,
-- (1) also define ev on Assertions
-- (2) also define ev on Constraints
evAssertion :: Expr Assertion -> [Expr Event]
evAssertion (EAssertionEvent e) = [e]
evAssertion (EAssertionNEvent e) = [e]
evAssertion (EAssertionConstraint c) = evConstraint c
evAssertion (EAssertionAnd a1 a2) = evAssertion a1 ++ evAssertion a2
evAssertion (EAssertionImplies e a) = e : evAssertion a

evConstraint :: Expr Constraint -> [Expr Event]
evConstraint (EConstraintCommunicates e1 e2) = [e1, e2]
evConstraint (EConstraintHappens e1 e2) = [e1, e2]

{-
 - SUBSECTION GLOBAL SPEC -> PER PARTY SPEC
 -}
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
projectPartyToEndpoint (EPartyProtocolSend (EChannel c1) i v f) (EChannel c)
  | c == c1 = EEndpointProtocolSend (EChannel c) i v f
  | otherwise = EEndpointProtocolEmp
projectPartyToEndpoint (EPartyProtocolReceive (EChannel c1) i v f) (EChannel c)
  | c == c1 = EEndpointProtocolReceive (EChannel c) i v f
  | otherwise = EEndpointProtocolEmp
projectPartyToEndpoint (EPartyProtocolConcurrency g1 g2) c =
  EEndpointProtocolConcurrency
    (projectPartyToEndpoint g1 c)
    (projectPartyToEndpoint g2 c)
projectPartyToEndpoint (EPartyProtocolChoice g1 g2) c =
  EEndpointProtocolChoice
    (projectPartyToEndpoint g1 c)
    (projectPartyToEndpoint g2 c)
projectPartyToEndpoint (EPartyProtocolSequencing g1 g2) c =
  EEndpointProtocolSequencing
    (projectPartyToEndpoint g1 c)
    (projectPartyToEndpoint g2 c)
projectPartyToEndpoint EPartyProtocolEmp _ = EEndpointProtocolEmp
