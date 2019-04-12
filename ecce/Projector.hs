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
  , Assertion(EAssertionConstraint, EAssertionEvent, EAssertionImplies,
          EAssertionNEvent, EOpAssertionBinary)
  , Channel
  , Constraint
  , Constraint(EConstraintCommunicates, EConstraintHappens)
  , EndpointProtocol(EEndpointProtocolEmp, EEndpointProtocolReceive,
                 EEndpointProtocolSend, EOpEndpointProtocolBinary)
  , EndpointProtocol
  , Event
  , Event(EEvent)
  , GlobalProtocol(EGlobalProtocolAssumption, EGlobalProtocolEmp,
               EGlobalProtocolGuard, EGlobalProtocolTransmission,
               EOpGlobalProtocolBinary)
  , GlobalProtocol
  , OpAssertionBinary(EAssertionAnd)
  , OpEndpointProtocolBinary(EEndpointProtocolChoice,
                         EEndpointProtocolConcurrency, EEndpointProtocolSequencing)
  , OpGlobalProtocolBinary(EGlobalProtocolChoice,
                       EGlobalProtocolConcurrency, EGlobalProtocolSequencing)
  , OpPartyProtocolBinary(EPartyProtocolChoice,
                      EPartyProtocolConcurrency, EPartyProtocolSequencing)
  , PartyProtocol(EOpPartyProtocolBinary, EPartyProtocolEmp,
              EPartyProtocolReceive, EPartyProtocolSend)
  , PartyProtocol
  , Role
  )

{-
 - SECTION Formal Definitions 4.2.1
 -}
{-
 - SUBSECTION TRANSMISSION
 -}
tr :: GlobalProtocol -> [GlobalProtocol]
tr g =
  case g of
    EGlobalProtocolTransmission _ _ _ _ _ _ -> [g]
    EOpGlobalProtocolBinary g1 EGlobalProtocolConcurrency g2 -> tr g1 ++ tr g2
    EOpGlobalProtocolBinary g1 EGlobalProtocolChoice g2 -> tr g1 ++ tr g2
    EOpGlobalProtocolBinary g1 EGlobalProtocolSequencing g2 -> tr g1 ++ tr g2
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
ev :: GlobalProtocol -> [Event]
ev g =
  case g of
    EGlobalProtocolTransmission s i r _ _ _ -> [EEvent s i, EEvent r i]
    EOpGlobalProtocolBinary g1 EGlobalProtocolConcurrency g2 -> ev g1 ++ ev g2
    EOpGlobalProtocolBinary g1 EGlobalProtocolChoice g2 -> ev g1 ++ ev g2
    EOpGlobalProtocolBinary g1 EGlobalProtocolSequencing g2 -> ev g1 ++ ev g2
    EGlobalProtocolAssumption a -> evAssertion a
    EGlobalProtocolGuard a -> evAssertion a
    EGlobalProtocolEmp -> []

-- Paper defines ev on Assumptions and Guards, so,
-- (1) also define ev on Assertions
-- (2) also define ev on Constraints
evAssertion :: Assertion -> [Event]
evAssertion a =
  case a of
    EAssertionEvent e -> [e]
    EAssertionNEvent e -> [e]
    EAssertionConstraint c -> evConstraint c
    EOpAssertionBinary a1 EAssertionAnd a2 -> evAssertion a1 ++ evAssertion a2
    EAssertionImplies e a -> e : evAssertion a

evConstraint :: Constraint -> [Event]
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
projectGlobalToParty :: GlobalProtocol -> Role -> PartyProtocol
projectGlobalToParty g p =
  case g of
    EGlobalProtocolTransmission s i r c v f
      | p == s -> EPartyProtocolSend c i v f
      | p == r -> EPartyProtocolReceive c i v f
      | otherwise -> EPartyProtocolEmp
    EOpGlobalProtocolBinary g1 EGlobalProtocolConcurrency g2 ->
      EOpPartyProtocolBinary
        (projectGlobalToParty g1 p)
        EPartyProtocolConcurrency
        (projectGlobalToParty g2 p)
    EOpGlobalProtocolBinary g1 EGlobalProtocolChoice g2 ->
      EOpPartyProtocolBinary
        (projectGlobalToParty g1 p)
        EPartyProtocolChoice
        (projectGlobalToParty g2 p)
    EOpGlobalProtocolBinary g1 EGlobalProtocolSequencing g2 ->
      EOpPartyProtocolBinary
        (projectGlobalToParty g1 p)
        EPartyProtocolSequencing
        (projectGlobalToParty g2 p)
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
projectPartyToEndpoint :: PartyProtocol -> Channel -> EndpointProtocol
projectPartyToEndpoint p c =
  case p of
    EPartyProtocolSend c1 i v f
      | c == c1 -> EEndpointProtocolSend i v f
      | otherwise -> EEndpointProtocolEmp
    EPartyProtocolReceive c1 i v f
      | c == c1 -> EEndpointProtocolReceive i v f
      | otherwise -> EEndpointProtocolEmp
    EOpPartyProtocolBinary g1 EPartyProtocolConcurrency g2 ->
      EOpEndpointProtocolBinary
        (projectPartyToEndpoint g1 c)
        EEndpointProtocolConcurrency
        (projectPartyToEndpoint g2 c)
    EOpPartyProtocolBinary g1 EPartyProtocolChoice g2 ->
      EOpEndpointProtocolBinary
        (projectPartyToEndpoint g1 c)
        EEndpointProtocolChoice
        (projectPartyToEndpoint g2 c)
    EOpPartyProtocolBinary g1 EPartyProtocolSequencing g2 ->
      EOpEndpointProtocolBinary
        (projectPartyToEndpoint g1 c)
        EEndpointProtocolSequencing
        (projectPartyToEndpoint g2 c)
    EPartyProtocolEmp -> EEndpointProtocolEmp
