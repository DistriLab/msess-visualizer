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
import Backend
  ( Expr(EGlobalProtocolChoice, EGlobalProtocolConcurrency,
     EGlobalProtocolEmp, EGlobalProtocolSequencing,
     EGlobalProtocolSequencing, EGlobalProtocolTransmission,
     EPartyProtocolAssumption, EPartyProtocolChoice,
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
