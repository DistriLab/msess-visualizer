\defslide{ProjectorIntroduction}{
Decompose big structures onto small structures, sometimes forgetting some
information.
}

%if False
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
\end{code}
%endif

%if False
\begin{code}
{-
 - SECTION Formal Definitions 4.2.1
 -}
{-
 - SUBSECTION TRANSMISSION
 -}
\end{code}
%endif

\defslide{ProjectorTrDesc}{
|tr| decomposes a |GlobalProtocol| into many |Transmission|s.  There are three
patterns:
\begin{enumerate}
  \item The pattern of decomposition for |Concurrency|, |Choice|, and
  |Sequencing|.  These are binary deconstructors (indicated by
  |EOpGlobalProtocolBinary|), so |tr| takes the two |GlobalProtocol|s on both
  sides of the deconstructor, then further processes them.
  \item |Assumption|, |Guard|, and |Emp| are ignored, because they do not
  contain any |Transmission|s.
  \item |tr| returns an order-preserved list.
\end{enumerate}
}

\defslide{ProjectorTrCode}{
\begin{code}
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
\end{code}
}

%if False
\begin{code}
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
\end{code}
%endif

\defslide{ProjectorEv}{
|ev| decomposes a |GlobalProtocol| in a similar manner to |tr|.

\begin{code}
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
\end{code}
}

%if False
Decompositions to lists of |Event|s are defined similarly for
|Assertion|s and |Constraint|s.  We will not be discussing them
here.
%endif

%if False
\begin{code}
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
\end{code}
%endif

%if False
\begin{code}
{-
 - SUBSECTION GLOBAL SPEC -> PER PARTY SPEC
 -}
\end{code}
%endif

%if False
|projectGlobalToParty| rewraps |GlobalProtocol| to
|PartyProtocol|, by recursively using |GlobalProtocol|
deconstructors to get the operands, then wrapping those operands with a
|PartyProtocol| constructor.  For readability, we only show one.
\par
The interesting case is when |projectGlobalToParty| takes a
|Transmission| as input, where the behavior of
|projectGlobalToParty| depends on the role it gets as input.  If the
role is a sender, then it constructs a |PartySend| event.  If the role
is a receiver, then it constructs a |PartyReceive| event.  Otherwise, it
constructs an empty |Party| event.
\par
\begin{code}
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
\end{code}
%endif

%if False
\begin{code}
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
\end{code}
%endif

%if False
\begin{code}
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
\end{code}
%endif

%if False
|projectPartyToEndpoint| behaves similarly to
|projectGlobalToParty|, except that it projects on |Channel|s
rather than |Role|s.  This function deconstructs and looks at the
channel of a |PartySend|, and constructs an |EndpointSend| event
only if its input channel matches.  Otherwise, no |Endpoint| event will
be constructed.  |PartyReceive| is handled similarly to construct
|EndpointReceive|.  We again exclude the uninteresting projections.

\begin{code}
projectPartyToEndpoint :: PartyProtocol -> Channel -> EndpointProtocol
projectPartyToEndpoint p c =
  case p of
    EPartyProtocolSend c1 i v f
      | c == c1 -> EEndpointProtocolSend i v f
      | otherwise -> EEndpointProtocolEmp
    EPartyProtocolReceive c1 i v f
      | c == c1 -> EEndpointProtocolReceive i v f
      | otherwise -> EEndpointProtocolEmp
\end{code}
%endif

%if False
\begin{code}
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
\end{code}
%endif

\section{Projector}
\slide{ProjectorIntroduction}
\slide{ProjectorTrDesc}
\slide{ProjectorTrCode}
\slide{ProjectorEv}
