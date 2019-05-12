\subsection{Processor}

With our |GlobalProtocol| deconstructors, we can actually start to deconstruct
them.  But first, we need to wrap the deconstructed |GlobalProtocol|s in
another data structure, |Process|, so that we do not lose information.

%if False
\begin{code}
{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows patterm match on GADT
{-# LANGUAGE RecursiveDo #-} -- Allows mdo
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns

{-
 - SECTION MODULE
 -}
module Processor where

{-
 - SECTION IMPORTS
 -}
import Base (extractParse)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import Data.Functor ((<$), (<$>))
import Data.List (intercalate, nub)
import Data.Maybe (fromJust)
import Interpreter (Output, mainHaskeline)
import Parser
  ( Channel(EChannel)
  , EndpointProtocol(EEndpointProtocolReceive, EEndpointProtocolSend)
  , GlobalProtocol(EGlobalProtocolEmp, EGlobalProtocolTransmission,
               EOpGlobalProtocolBinary)
  , GlobalProtocol
  , OpGlobalProtocolBinary(EGlobalProtocolChoice,
                       EGlobalProtocolConcurrency, EGlobalProtocolSequencing)
  , Role(ERole)
  , extractFile
  , parseGlobalProtocol
  )
import qualified Parser (Event(EEvent))
import Projector (ev, projectGlobalToParty, projectPartyToEndpoint, tr)
import Reactive.Banana
  ( Behavior
  , Event
  , Moment
  , (<@)
  , (<@>)
  , accumB
  , compile
  , filterE
  , filterJust
  , liftMoment
  , mapAccum
  , unionWith
  , whenE
  )
import Reactive.Banana.Frameworks
  ( EventNetwork
  , Handler
  , MomentIO
  , actuate
  , changes
  , fromAddHandler
  , newAddHandler
  , reactimate
  , reactimate'
  )
import System.IO (FilePath)
import Unparser (un)

{-
 - SECTION USER INTERFACE
 -}

main :: IO ()
main = mainHaskeline commandOutputs incommandOutput

commandOutputs :: [(String, Output)]
commandOutputs =
  [ ( "help"
    , \(_, commands, _) ->
        mapM_ putStrLn $ "Here are a list of commands:" : commands)
  , ( "load"
    , \(_, _, restInputLine) ->
        (do (addKeyEvent, fireKey) <- newAddHandler
            network <-
              compile $
              fromAddHandler addKeyEvent >>= networkDescription restInputLine
            actuate network
            eventLoop fireKey network))
  , ( "test"
    , \(_, _, restInputLine) ->
        (do (addKeyEvent, fireKey) <- newAddHandler
            network <-
              compile $
              fromAddHandler addKeyEvent >>=
              networkDescription ("test/processor/" ++ restInputLine)
            actuate network
            eventLoop fireKey network))
  ]

incommandOutput :: Output
incommandOutput =
  \(_, commands, _) ->
    mapM_ putStrLn $ "Here are a list of commands:" : commands

-- Read commands and fire corresponding events
eventLoop :: Handler Char -> EventNetwork -> IO ()
eventLoop fireKey network = loop
  where
    loop = do
      putStr "> "
      c <- getChar
      putChar '\n'
      case c of
        'q' -> putStrLn "Quitting" >> return ()
        otherwise -> fireKey c
      when (c /= 'q') loop

commands :: [String]
commands = "help" : "load" : []

{-----------------------------------------------------------------------------
    Program logic
------------------------------------------------------------------------------}
\end{code}
%endif

A |Process| is either a single |GlobalProtocol|, or a list of other
|Process|es.  A sequential process resolves processes in order: resolve the
head |Process|, then resolve the tails.  A concurrent |Process| is resolved
without a defined order.  However, for ease of implementation, we resolve
concurrent |Process|es in order.

\begin{code}
data Process
  = Leaf GlobalProtocol
  | NodeS [Process]
  | NodeC [Process]
\end{code}
%if False
\begin{code}
  deriving (Show)
\end{code}
%endif

%if False
\begin{code}
-- SECTION NETWORK
\end{code}
%endif

|networkDescription| reads a |GlobalProtocol| from a file at |filePath|.
\textit{reactive-banana} provides the user input as asingle character, |eKey|,
which |networkDescription| passes on to |networkProcessor|, before passing the
processed result to |networkPrinter|.

\begin{code}
networkDescription :: FilePath -> Event Char -> MomentIO ()
networkDescription filePath eKey =
  (liftIO $ extractFile filePath) >>=
  (\xs ->
     liftMoment $
     networkProcessor (fmap head (parseContents xs)) (Just <$> eKey)) >>=
  networkPrinter
\end{code}

|networkProcessor| behaves differently based on the value of |eKey|.  There are
two types of inputs a user may make: either |'s'| to step the processor, or
|'1'| to choose the left side of a |GlobalChoice|, or |'2'| to choose the right
side of a |GlobalChoice|.
\par
|networkProcessor| is defined as a recursive monadic-do, meaning there is some
cycle in the sequential composition of monads (Fig. TODO).
\\\begin{center}
\includegraphics[scale=0.4]{../ecce/plantuml/processor.png}
\end{center}

|networkProcessor| also uses |Event|s and |Behavior|s from
\textit{reactive-banana}.  We give a rough overview of
\textit{reactive-banana}.  This overview is merely conceptual, the underlying
implementation of \textit{reactive-banana} could be different.
\textit{reactive-banana} is a functional reactive programming framework, that
processes data streams.  Data streams are lists with potentially infinite
elements.  Each element can be understood as a 2-tuple of |Time| and |Data|,
meaning the |Data| was generated at time |Time|.  An event is a data stream.  A
behavior is a description of how |Data| varies with |Time|.
\par
|Event|s are used like data streams, and |Behavior|s are used to store a
snapshot of an |Event|: it allows for data persistence.  As such, we refer to
|Event|s as if they were the data streams itself, and we refer to |Behavior|s
as if they were the data itself.  |networkProcessor| is defined in terms of
many small |Event|s and |Behavior|s to allow ease of understanding and enable
UNIX-style composition of |Event|s and |Behavior|s.  This makes modifications
easier than if the |Processor| was defined in a monolithic fashion.  As a
convention, variables containing |Event|s have names prefixed with |e|, and
variables containing |Behavior|s have names prefixed with |b|.

%if False
\begin{code}
networkProcessor ::
     Maybe Process
  -> Event (Maybe Char)
  -> Moment ( Event (Maybe GlobalProtocol)
            , Behavior (Maybe Process)
            , Event ()
            , Behavior Int)
\end{code}
%endif
\begin{code}
networkProcessor p eKey
\end{code}
%if False
\begin{code}
      -- SUBSECTION USER INPUT
      -- bProcChoiceMay:
      --    looks at bProc to see if current process is EGlobalProtocolChoice
      -- eProcChoice: process selected by user, fires on eChooserChoice
      -- eChooseMay:
      --    only fires if user may choose
      -- eChooserChoice: choice selected by user, fires on eChooseMay
      -- bProcChoiceFunc:
      --    to be applied to eChooserChoice, to generate eProcChoice
      --    looks at bProcChoiceMay to get list of processes to be chosen from
      --    returns selected process from that list of processes
      --    always guaranteed to have [Process], not Maybe [Process]
      --        because of how eChooseMay guarantees bProcChoiceMay will always
      --        be (Just ...)
\end{code}
%endif

%if False
\begin{code}
 =
\end{code}
%endif

Due to the circular nature of the network, we need to make an assumption in
order to explain the rest of the network.  We assume that the |Behavior|
|bProc| is the list of remaining |Process|es.

|bProcChoiceMay| looks at the data inside |bProc|, and if the data is a
|GlobalChoice|, |bProcChoiceMay| converts that data into a 2-element list of
the first choice |g1| and the second choice |g2|.

\begin{code}
  mdo let bProcChoiceMay :: Behavior (Maybe [Process])
          bProcChoiceMay =
            ((\x ->
                case x of
                  Just (NodeS (Leaf (EOpGlobalProtocolBinary g1 EGlobalProtocolChoice g2):_)) ->
                    Just [Leaf g1, Leaf g2]
                  otherwise -> Nothing) <$>
             bProc)
\end{code}

|bProcChoiceFunc| is a function that maps |Char|s into |Process|es.  |'1'| is
converted into index 0, and |'2'| is converted into index 1.  |bProcChoiceFunc|
uses these indexes to index into |bProcChoiceMay|.  Therefore, it maps |'1'| to
|g1|, and |'2'| to |g2|.

\begin{code}
          bProcChoiceFunc :: Behavior (Char -> Process)
          bProcChoiceFunc =
            flip ((flip (!!)) . (subtract (fromEnum '1')) . fromEnum) . fromJust <$>
            bProcChoiceMay
\end{code}

|eProcChoice| applies |bProcChoiceFunc| on the stream of |eChooserChoice|.  The
resulting stream is the |Process|es chosen by the user: either |g1| or |g2|.

\begin{code}
          eProcChoice :: Event Process
          eProcChoice = bProcChoiceFunc <@> eChooserChoice
\end{code}

|bProcIsChoice| is |True| only if |bProcChoiceMay| contains the choice of |g1|
and |g2|.  Otheriwse, |bProcIsChoice| is |False|.

\begin{code}
          bProcIsChoice :: Behavior Bool
          bProcIsChoice = maybe False (const True) <$> bProcChoiceMay
\end{code}

|eChooseMay| is identical to |eKey| only when |bProcIsChoice| is |True|.
Otheriwse, |eChooseMay| is an empty stream.  In other words, |eChooseMay|
allows us to look at the |eKey| stream only when the list of remaining
|Process|es is a|GlobalChoice|.  This restricts processing of user inputs to
only when |bProc| is a |GlobalChoice|.

\begin{code}
          eChooseMay :: Event Char
          eChooseMay = whenE bProcIsChoice (filterJust eKey)
\end{code}

|eChooseMayNot| is similar to |eChooseMay|: it is identical to |eKey| only when
|bProcIsChoice| is |False|.  Otheriwse, |eChooseMayNot| is an empty stream.  In
other words, |eChooseMayNot| allows us to look at the |eKey| stream only when
the list of remaining |Process|es is not a |GlobalChoice|.  This restricts
processing of user inputs to only when |bProc| is not a |GlobalChoice|.

\begin{code}
          eChooseMayNot :: Event Char
          eChooseMayNot = whenE (not <$> bProcIsChoice) (filterJust eKey)
\end{code}

The partitioned scopes of |eChooseMay| and |eChooseMayNot| ensures that further
processing of data will be restricted to only either one of those scopes.  This
is helpful in reasoning about the behavior of the network.
\par
|eStepper| is the stream of |eChooseMayNot| when |eChooseMayNot| has |'s'| in
its data stream.  |'s'| is the character the user gives as input, in order to
step the processor.

\begin{code}
          eStepper :: Event Char
          eStepper = filterE (== 's') eChooseMayNot
\end{code}

|eDigit| is the stream of |eChooseMay| when |eChooseMay| has any digits in its
data stream.

\begin{code}
          eDigit :: Event Char
          eDigit = filterE isDigit eChooseMay
\end{code}

|eChooserChoice| is the stream of |'1'|s and |'2'|s inside |eDigit|.  |'1'|s
and |'2'|s are the characters the user gives as input, to choose |g1| or |g2|
respectively.

\begin{code}
          eChooserChoice :: Event Char
          eChooserChoice = filterE (`elem` "12") eDigit
\end{code}

%if False
\begin{code}
      -- SUBSECTION STEPPER
      -- xs: contents of file
      -- (eTrans, bProc): tuple of two elements:
      --    (1) global transmission that the debugger output
      --    (2) process that the debugger currently has
      --    processStep:
      --        Ignore the accumulated bProc
      --        Take in the new bProc
\end{code}
%endif

Here is the function that completes the circular monadic sequential
composition.  This function processes each step of the input |Process| |p|, and
accumulates the results inside |eTrans| and |bProc|.  Processing handled by the
|processStep| function.
\par
|unionWith| merges two incoming streams into one.  We know that this merged
stream has no duplicates, because the two incoming streams, |eProcChoice| and
|eStepper|, have disjoint scopes.
\par
For the |eProcChoice| stream, we just take the |Process| inside |eProcChoice|,
and pass it on to |processStep|.  Processing for the |eStepper| stream is
different: when the user triggers a step, the current |bProc| is passed to
|processStep|, and |processStep| returns an updated |bProc|.
\par
To discharge our assumption when we first started analyzing this circular
composition, that |bProc| is the list of remaining |Process|es, |processStep|
needs to return the correct |bProc|.  Note that the initial |bProc| is given by
the input |Process| |p|, and we assume that this initial |p| satisfies our
assumption.  This is areasonable assumption to make, because it is reasonable
to have undefined behavior when inputs to functions are not well-formed.  We
defer the rest of the analysis (of |processStep|) to the end of this section.
\par
\begin{code}
      (eTrans :: Event (Maybe GlobalProtocol), bProc :: Behavior (Maybe Process)) <-
        mapAccum p $
        unionWith
          const
          (const <$> ((processStep . Just) <$> eProcChoice))
          (const <$> (processStep <$> bProc <@ eStepper))
\end{code}
%if False
\begin{code}
      -- SUBSECTION STEPPER STATE
      -- bStepCount: number of eSteppers fired
      -- eDone: whether the debugger is done
\end{code}
%endif

|bStepCount| is the number of steps the user made so far.  It accumulates an
integer with initial value zero, and applies an increment function |(+ 1)|
whenever |bProc| has a |Just| value, and the user makes a step.  This means
that, when we are done processing, the input |Process| |p| is completely
consumed, and |bProc| will have |Nothing|.  In this case, the increment
function will not be applied.

\begin{code}
      (bStepCount :: Behavior Int) <-
        accumB
          0
          ((+ 1) <$ whenE ((maybe False (const True)) <$> bProc) eStepper)
\end{code}

|eDone| indicates that |bProc| is completely consumed.  Note the similarities
in implementation as compared to |bStepCount|, except that the data in
|eStepper| is converted to the unit value |()| before further processing.  This
ensures that |eDone| will have a type of |Event ()|.  We use the unit type
because we only need an event to indicate that processing is done.

\begin{code}
      let eDone :: Event ()
          eDone = whenE ((maybe True (const False)) <$> bProc) (() <$ eStepper)
\end{code}

Lastly, |networkDescription| returns a 4-tuple |(eTrans, bProc, eDone,
bStepCount)| to be used by |networkPrinter| and external modules (like
|networkTransmit| in |Frontend.lhs|).

\begin{code}
      return (eTrans, bProc, eDone, bStepCount)
\end{code}

|networkPrinter| allows the processor to be run on the textual interface
defined in |Interpreter.lhs|.  We print many things:
\begin{enumerate}
  \item Transmissions from |eTrans|,
  \item Changes in |bProc|, which are first converted to |GlobalProtocol|s by
  the |mayProcessToGlobalProtocol| function (more about this function later),
  then projected to endpoint transmissions.  Since the unparsing function |un|
  takes an input of type |GlobalProtocol| as input, we have to convert
  |EndpointSend|s and |EndpointReceive|s back into |GlobalTransimission|s, with
  some blank fields.
  \item Changes in step count, and
  \item |"Done!"| to indicate that the processor has completely consumed
  |bProc|.
\end{enumerate}

\begin{code}
networkPrinter ::
     ( Event (Maybe GlobalProtocol)
     , Behavior (Maybe Process)
     , Event ()
     , Behavior Int)
  -> MomentIO ()
networkPrinter (eTrans, bProc, eDone, bStepCount) = do
  reactimate $
    maybe (return ()) (putStrLn . ("Transmission: " ++) . un) <$> eTrans
  eProc <- changes bProc
  reactimate' $
    fmap
      (putStrLn .
       intercalate "\n" .
       nub .
       map
         (un .
          (\x ->
             case x of
               EEndpointProtocolSend i v f ->
                 EGlobalProtocolTransmission
                   (ERole "")
                   i
                   (ERole "")
                   (EChannel "")
                   v
                   f
               EEndpointProtocolReceive i v f ->
                 EGlobalProtocolTransmission
                   (ERole "")
                   i
                   (ERole "")
                   (EChannel "")
                   v
                   f)) .
       projectGlobalToEndpoint . mayProcessToGlobalProtocol) <$>
    eProc
  eStepCount <- changes bStepCount
  reactimate' $ fmap (putStrLn . show) <$> eStepCount
  reactimate $ putStrLn "Done!" <$ eDone
\end{code}

%if False
\begin{code}
projectGlobalToEndpoint :: GlobalProtocol -> [EndpointProtocol]
projectGlobalToEndpoint g =
  [ projectPartyToEndpoint (projectGlobalToParty g p) c
  | p <- partiesInGlobalProtocol g
  , c <- channelsInGlobalProtocol g
  ]

partiesInGlobalProtocol :: GlobalProtocol -> [Role]
partiesInGlobalProtocol g = [p | Parser.EEvent p _ <- ev g]

channelsInGlobalProtocol :: GlobalProtocol -> [Channel]
channelsInGlobalProtocol g =
  [c | EGlobalProtocolTransmission _ _ _ c _ _ <- tr g]
\end{code}
%endif

|mayProcessToGlobalProtocol| returns a |EGlobalProtocolEmp| if its input is
|Nothing|.  Otherwise, it applies a fixpoint function on the input: this
fixpoint function is accessible from within itself as |r|.  We deconstruct
|Leaf| |Process|es in the expected manner.  We deconstruct the other
|Process|es, |NodeS|s and |NodeC|s, by recursing on only the head of the
contained lists.  If the list is empty, then the fixpoint function returns
|EGlobalProtocolEmp|.

\begin{code}
mayProcessToGlobalProtocol :: Maybe Process -> GlobalProtocol
mayProcessToGlobalProtocol =
  maybe
    EGlobalProtocolEmp
    (fix
       (\r p ->
          case p of
            NodeS [] -> EGlobalProtocolEmp
            NodeC [] -> EGlobalProtocolEmp
            NodeS (p:ps) -> r p
            NodeC (p:ps) -> r p
            Leaf g -> g))
\end{code}

%if False
\begin{code}
parseContents :: Either [String] [String] -> Maybe [Process]
parseContents xs =
  either
    (\e -> error $ "Parse error: " ++ show e)
    (\xs ->
       let gs = head $ map (extractParse parseGlobalProtocol) xs
        in Just $ map Leaf gs)
    xs
\end{code}
%endif

|processStep| deconstructs a single level of |Process|, to return a tuple of
|Maybe GlobalProtocol| and |Maybe Process|.  This |GlobalProtocol| is actually
a |GlobalTransmission|, and the returned |Process| is the rest of the
unprocessed |Process|es.  This way of interpretation draws parallels between
|procesStep| and the model of a debugger.  We now explain how |processStep|
derives its return value.
\par
Firstly, we analyze |Maybe GlobalProtocol| return value.  When deconstructing
with binary operators, |Maybe GlobalProtocol| is |Nothing|, as deconstructing a
single level of |Process| does not yield any transmissions.  However, for all
other types of |GlobalProtocol|s, and in the recursive calls to |processStep|,
some |GlobalTransmission| is yielded.  We do not handle |GlobalChoice| here,
since it is already handled by |networkProcessor|.
\par
Secondly, we analyze the |Maybe Process| return value.  We apply induction on
the structure of |Process|, with an induction hypothesis that the |Maybe
Process| return value of the subsequent call to |processStep| is well-formed.
We split our analysis by cases.  If the input |Process| |p| is a |Leaf|, then
the |Process| return value is a well-formed process, since the |Process|
constructors |NodeC| and |NodeS| are used.  Otherwise, if the input |p ::
Process| is a |NodeS| or |NodeC|, then we deconstruct the node into its
well-formed head |p :: Maybe Process| and tail |ps :: [Maybe Process]|.  We
next take the |p' :: Maybe Process| from the subsequent call to |processStep|,
which we know is well-formed by our induction hypothesis.  Then, we form |ps'
:: [Maybe Process]| by prepending |p'| to |ps| only if |p'| is not |Nothing|.
Since both |p'| and |ps| are well-formed, thus both |NodeS ps'| and |NodeC ps'|
are also well-formed.
\par
We now fully discharge the assumption that we had at the beginning of our
circular analysis of |networkProcessor|.  Remember that we reasonably claimed
that the input |Process| to |networkProcessor| satisfies our assumption.  Now,
we can observe that the inductive step is true, that subsequent |Maybe Process|
are also well-formed.  This stems from what we have shown, that |processStep|
returns a well-formed |Maybe Process|.
\par
To observe that the returned |Maybe Process| is the list of remaining
unprocessed processes, just note that for each call to |processStep|,
processing is done only on the head of the input |Process|, and the results
from that processing is prepended onto the tail of the input |Process|.
\par
This circular nature of the network is strongly reminiscent of Hughes'
|ArrowLoop| class, where signals output from the |Arrow| are fed back as input.
Indeed, if we define \textit{reactive-banana} behavior transformations as
|Arrow|s, then we would be able to replace the circular network with an
|ArrowLoop|.  However, choosing this more imperative style eases explanation,
and the reader does not need to understand arrows in order to understand the
\textit{reactive-banana} network.

\begin{code}
processStep :: Maybe Process -> (Maybe GlobalProtocol, Maybe Process)
processStep p =
  case p of
    Nothing -> (Nothing, Nothing)
    Just p ->
      case p of
        Leaf g ->
          case g of
            EOpGlobalProtocolBinary g1 EGlobalProtocolConcurrency g2 ->
              (Nothing, Just $ NodeC [Leaf g1, Leaf g2])
            EOpGlobalProtocolBinary g1 EGlobalProtocolSequencing g2 ->
              (Nothing, Just $ NodeS [Leaf g1, Leaf g2])
            otherwise -> (Just g, Nothing)
        NodeS [] -> (Nothing, Nothing)
        NodeS (p:ps) -> (s', Just $ NodeS ps')
          where (s', p') = processStep (Just p)
                ps' = maybe ps (: ps) p'
        NodeC [] -> (Nothing, Nothing)
        NodeC (p:ps) -> (s', Just $ NodeC ps')
          where (s', p') = processStep (Just p)
                ps' = maybe ps (: ps) p'
\end{code}
