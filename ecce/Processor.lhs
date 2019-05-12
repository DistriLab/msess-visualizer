\defslide{ProcessorIntroduction}{
\begin{itemize}
  \item Wrap deconstructed |GlobalProtocol|s in |Process| datatype.
  \item Deconstruct with |GlobalProtocol| constructors.
\end{itemize}
}

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

\defslide{ProcessorProcess}{
A |Process| is either a single |GlobalProtocol|, or a list of other
|Process|es.

\begin{code}
data Process
  = Leaf GlobalProtocol
  | NodeS [Process]
  | NodeC [Process]
\end{code}
}

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

%if False
|networkDescription| reads a |GlobalProtocol| from a file at
|filePath|.  |reactive-banana| provides the user input as a
single character, |eKey|, which |networkDescription| passes on to
|networkProcessor|, before passing the processed result to
|networkPrinter|.

\begin{code}
networkDescription :: FilePath -> Event Char -> MomentIO ()
networkDescription filePath eKey =
  (liftIO $ extractFile filePath) >>=
  (\xs ->
     liftMoment $
     networkProcessor (fmap head (parseContents xs)) (Just <$> eKey)) >>=
  networkPrinter
\end{code}
%endif

\defslide{ProcessorInputs}{
|networkProcessor| behaves differently based on user inputs:
\begin{itemize}
  \item |'s'| to step the processor,
  \item |'1'| to choose the left side of a |GlobalChoice|,
  \item |'2'| to choose the right side of a |GlobalChoice|.
\end{itemize}
}

\defslide{ProcessorDiagram}{
|networkProcessor| is a recursive monadic-do, there is some cycle.
\\\begin{center}
\includegraphics[scale=0.2]{../ecce/plantuml/processor.png}
\end{center}
}

%if False
|networkProcessor| also uses |Event|s and |Behavior|s from
|reactive-banana|.  We give a rough overview of
|reactive-banana|.  This overview is merely conceptual, the underlying
implementation of |reactive-banana| could be different.
|reactive-banana| is a functional reactive programming framework, that
processes data streams.  Data streams are lists with potentially infinite
elements.  Each element can be understood as a 2-tuple of |Time| and
|Data|, meaning the |Data| was generated at time |Time|.
An event is a data stream.  A behavior is a description of how |Data|
varies with |Time|.
\par
|Event|s are used like data streams, and |Behavior|s are used to
store a snapshot of an |Event|: it allows for data persistence.  As
such, we refer to |Event|s as if they were the data streams itself, and
we refer to |Behavior|s as if they were the data itself.
|networkProcessor| is defined in terms of many small |Event|s and
|Behavior|s to allow ease of understanding and enable UNIX-style
composition of |Event|s and |Behavior|s.  This makes
modifications easier than if the |Processor| was defined in a monolithic
fashion.  As a convention, variables containing |Event|s have names
prefixed with |e|, and variables containing |Behavior|s have
names prefixed with |b|.
%endif

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
\begin{code}
networkProcessor p eKey
\end{code}
%endif

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

%if False
Due to the circular nature of the network, we need to make an assumption in
order to explain the rest of the network.  We assume that the |Behavior|
|bProc| is the list of remaining |Process|es.
%endif

\defslide{ProcessbProcChoiceMay}{
If the data in |bProc| is a |GlobalChoice|, |bProcChoiceMay| splits that into
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
}

\defslide{ProcessorBProcChoiceFunc}{
|bProcChoiceFunc|:
\begin{itemize}
  \item maps |Char|s to |Process|es,
  \item indexes into |bProcChoiceMay| by ASCII,
  \item maps |'1'| to |'g1'|, and |'2'| to |'g2'|.
\end{itemize}

\begin{code}
          bProcChoiceFunc :: Behavior (Char -> Process)
          bProcChoiceFunc =
            flip ((flip (!!)) . (subtract (fromEnum '1')) . fromEnum) . fromJust <$>
            bProcChoiceMay
\end{code}
}

\defslide{ProcessorEProcChoice}{
|eProcChoice| applies |bProcChoiceFunc| on the stream of |eChooserChoice|.  The
resulting stream is the |Process|es chosen by the user: either |g1| or |g2|.

\begin{code}
          eProcChoice :: Event Process
          eProcChoice = bProcChoiceFunc <@> eChooserChoice
\end{code}
}

\defslide{ProcessorBProcIsChoice}{
|bProcIsChoice| is |True| only if |bProcChoiceMay| contains the choice of |g1|
and |g2|.  Otheriwse, |bProcIsChoice| is |False|.

\begin{code}
          bProcIsChoice :: Behavior Bool
          bProcIsChoice = maybe False (const True) <$> bProcChoiceMay
\end{code}
}

\defslide{ProcessorEChooseMay}{
|eChooseMay| allows us to look at the |eKey| stream only when the list of
remaining |Process|es is a |GlobalChoice|.

\begin{code}
          eChooseMay :: Event Char
          eChooseMay = whenE bProcIsChoice (filterJust eKey)
\end{code}
}

\defslide{ProcessorEChooseMayNot}{
|eChooseMayNot| allows us to look at the |eKey| stream only when the list of
remaining |Process|es is not a |GlobalChoice|.

\begin{code}
          eChooseMayNot :: Event Char
          eChooseMayNot = whenE (not <$> bProcIsChoice) (filterJust eKey)
\end{code}
}

\defslide{ProcessorEChooseScopes}{
The partitioned scopes of |eChooseMay| and |eChooseMayNot| ensures that further
stream processing is similarly partitioned.
}

\defslide{ProcessorEStepper}{
|eStepper| is the stream of |eChooseMayNot| when |eChooseMayNot| has |'s'| in
its data stream.

\begin{code}
          eStepper :: Event Char
          eStepper = filterE (== 's') eChooseMayNot
\end{code}
}

\defslide{ProcessorEDigit}{
|eDigit| is the stream of |eChooseMay| when |eChooseMay| has any digits in its
data stream.

\begin{code}
          eDigit :: Event Char
          eDigit = filterE isDigit eChooseMay
\end{code}
}

\defslide{ProcessorEChooserChoice}{
|eChooserChoice| is the stream of |'1'|s and |'2'|s inside |eDigit|.

\begin{code}
          eChooserChoice :: Event Char
          eChooserChoice = filterE (`elem` "12") eDigit
\end{code}
}

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

\defslide{ProcessETransBProc}{
\begin{itemize}
  \item |unionWith| merges two incoming streams into one, without duplicates.
  \item The two incoming streams, |eProcChoice| and |eStepper|, have disjoint
  scopes.
  \item To show that |bProc| is the list of remaining |Process|es,
  |processStep| needs to return the correct |bProc|.
\end{itemize}

\begin{code}
      (eTrans :: Event (Maybe GlobalProtocol), bProc :: Behavior (Maybe Process)) <-
        mapAccum p $
        unionWith
          const
          (const <$> ((processStep . Just) <$> eProcChoice))
          (const <$> (processStep <$> bProc <@ eStepper))
\end{code}
}

%if False
\begin{code}
      -- SUBSECTION STEPPER STATE
      -- bStepCount: number of eSteppers fired
      -- eDone: whether the debugger is done
\end{code}
%endif

\defslide{ProcessorBStepCount}{
|bStepCount| is the number of steps the user made so far.  When we are done
processing, the input |Process| |p| is completely consumed, and |bProc| will
have |Nothing|.

\begin{code}
      (bStepCount :: Behavior Int) <-
        accumB
          0
          ((+ 1) <$ whenE ((maybe False (const True)) <$> bProc) eStepper)
\end{code}
}

\defslide{ProcessorEDone}{
|eDone| indicates that |bProc| is completely consumed.  The data in |eStepper|
is converted to the unit value |()|.

\begin{code}
      let eDone :: Event ()
          eDone = whenE ((maybe True (const False)) <$> bProc) (() <$ eStepper)
\end{code}
}

\defslide{ProcessorNetworkDescription}{
|networkDescription| returns a 4-tuple |(eTrans, bProc, eDone, bStepCount)| to
be used by |networkPrinter| and external modules.

\begin{code}
      return (eTrans, bProc, eDone, bStepCount)
\end{code}
}

\defslide{ProcessorNetworkPrinterDesc}{
|networkPrinter| allows the processor to be run on the textual interface
defined in |Interpreter.lhs|.  We print many things:
\begin{enumerate}
  \item Transmissions from |eTrans|,
  \item Changes in |bProc|, which are first converted to
  |GlobalProtocol|s by the |mayProcessToGlobalProtocol| function,
  then projected to endpoint transmissions.
  \item Changes in step count,
  \item |"Done!"| indicates that the processor completely consumed
  |bProc|.
\end{enumerate}
}

%if False
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
%endif

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

%if False
|mayProcessToGlobalProtocol| returns a |EGlobalProtocolEmp| if
its input is |Nothing|.  Otherwise, it applies a fixpoint function on
the input: this fixpoint function is accessible from within itself as
|r|.  We deconstruct |Leaf| |Process|es in the expected
manner.  We deconstruct the other |Process|es, |NodeS|s and
|NodeC|s, by recursing on only the head of the contained lists.  If the
list is empty, then the fixpoint function returns |EGlobalProtocolEmp|.

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
%endif

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

\defslide{ProcessorProcessStepNothing}{
|processStep| deconstructs a single level of |Process|, to return a tuple of
|Maybe GlobalTransmission| and |Maybe Process|.

\begin{code}
processStep :: Maybe Process -> (Maybe GlobalProtocol, Maybe Process)
processStep p =
  case p of
    Nothing -> (Nothing, Nothing)
    Just p ->
      case p of
\end{code}
}

\defslide{ProcessorProcessStepLeaf}{
\begin{code}
        Leaf g ->
          case g of
            EOpGlobalProtocolBinary g1 EGlobalProtocolConcurrency g2 ->
              (Nothing, Just $ NodeC [Leaf g1, Leaf g2])
            EOpGlobalProtocolBinary g1 EGlobalProtocolSequencing g2 ->
              (Nothing, Just $ NodeS [Leaf g1, Leaf g2])
            otherwise -> (Just g, Nothing)
\end{code}
}

\defslide{ProcessorProcessStepNode}{
\begin{code}
        NodeS [] -> (Nothing, Nothing)
        NodeS (p:ps) -> (s', Just $ NodeS ps')
          where (s', p') = processStep (Just p)
                ps' = maybe ps (: ps) p'
        NodeC [] -> (Nothing, Nothing)
        NodeC (p:ps) -> (s', Just $ NodeC ps')
          where (s', p') = processStep (Just p)
                ps' = maybe ps (: ps) p'
\end{code}
}

\section{Processor}
\slide{ProcessorIntroduction}
\slide{ProcessorProcess}
\slide{ProcessorInputs}
\slide{ProcessorDiagram}
\slide{ProcessbProcChoiceMay}
\slide{ProcessorBProcChoiceFunc}
\slide{ProcessorEProcChoice}
\slide{ProcessorBProcIsChoice}
\slide{ProcessorEChooseMay}
\slide{ProcessorEChooseMayNot}
\slide{ProcessorEStepper}
\slide{ProcessorEDigit}
\slide{ProcessorEChooserChoice}
\slide{ProcessETransBProc}
\slide{ProcessorBStepCount}
\slide{ProcessorEDone}
\slide{ProcessorNetworkDescription}
\slide{ProcessorNetworkPrinterDesc}
\slide{ProcessorProcessStepNothing}
\slide{ProcessorProcessStepLeaf}
\slide{ProcessorProcessStepNode}
