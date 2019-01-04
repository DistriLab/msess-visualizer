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
  ( Channel
  , EndpointProtocol
  , Expr(EChannel, EEndpointProtocolChoice,
     EEndpointProtocolConcurrency, EEndpointProtocolEmp,
     EEndpointProtocolReceive, EEndpointProtocolSend,
     EEndpointProtocolSequencing, EGlobalProtocolChoice,
     EGlobalProtocolConcurrency, EGlobalProtocolEmp,
     EGlobalProtocolSequencing, EGlobalProtocolSequencing,
     EGlobalProtocolTransmission, EPartyProtocolChoice,
     EPartyProtocolConcurrency, EPartyProtocolEmp,
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
