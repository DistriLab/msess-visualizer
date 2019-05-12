\defslide{ProcessorIntroduction}{
\begin{itemize}
  \item Wrap deconstructed \textit{GlobalProtocol}s in \textit{Process}
  datatype.
  \item Deconstruct with \textit{GlobalProtocol} constructors.
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
A \textit{Process} is either a single \textit{GlobalProtocol}, or a list of
other \textit{Process}es.

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
\textit{networkDescription} reads a \textit{GlobalProtocol} from a file at
\textit{filePath}.  \textit{reactive-banana} provides the user input as a
single character, \textit{eKey}, which \textit{networkDescription} passes on to
\textit{networkProcessor}, before passing the processed result to
\textit{networkPrinter}.

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
\textit{networkProcessor} behaves differently based on user inputs:
\begin{itemize}
  \item \textit{'s'} to step the processor,
  \item \textit{'1'} to choose the left side of a \textit{GlobalChoice},
  \item \textit{'2'} to choose the right side of a \textit{GlobalChoice}.
\end{itemize}
}

\defslide{ProcessorDiagram}{
\textit{networkProcessor} is defined as a recursive monadic-do, meaning there
is some cycle in the sequential composition of monads.
\\\begin{center}
\includegraphics[scale=0.2125]{../ecce/plantuml/processor.png}
\end{center}
}

%if False
\textit{networkProcessor} also uses \textit{Event}s and \textit{Behavior}s from
\textit{reactive-banana}.  We give a rough overview of
\textit{reactive-banana}.  This overview is merely conceptual, the underlying
implementation of \textit{reactive-banana} could be different.
\textit{reactive-banana} is a functional reactive programming framework, that
processes data streams.  Data streams are lists with potentially infinite
elements.  Each element can be understood as a 2-tuple of \textit{Time} and
\textit{Data}, meaning the \textit{Data} was generated at time \textit{Time}.
An event is a data stream.  A behavior is a description of how \textit{Data}
varies with \textit{Time}.
\par
\textit{Event}s are used like data streams, and \textit{Behavior}s are used to
store a snapshot of an \textit{Event}: it allows for data persistence.  As
such, we refer to \textit{Event}s as if they were the data streams itself, and
we refer to \textit{Behavior}s as if they were the data itself.
\textit{networkProcessor} is defined in terms of many small \textit{Event}s and
\textit{Behavior}s to allow ease of understanding and enable UNIX-style
composition of \textit{Event}s and \textit{Behavior}s.  This makes
modifications easier than if the \textit{Processor} was defined in a monolithic
fashion.  As a convention, variables containing \textit{Event}s have names
prefixed with \textit{e}, and variables containing \textit{Behavior}s have
names prefixed with \textit{b}.
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
order to explain the rest of the network.  We assume that the \textit{Behavior}
\textit{bProc} is the list of remaining \textit{Process}es.
%endif

\defslide{ProcessbProcChoiceMay}{
\textit{bProcChoiceMay} looks at the data inside \textit{bProc}, and if the
data is a \textit{GlobalChoice}, \textit{bProcChoiceMay} converts that data
into a 2-element list of the first choice \textit{g1} and the second choice
\textit{g2}.

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
\textit{bProcChoiceFunc} is a function that maps \textit{Char}s into
\textit{Process}es.  \textit{'1'} is converted into index 0, and \textit{'2'}
is converted into index 1.  \textit{bProcChoiceFunc} uses these indexes to
index into \textit{bProcChoiceMay}.  Therefore, it maps \textit{'1'} to
\textit{'g1'}, and \textit{'2'} to \textit{'g2'}.

\begin{code}
          bProcChoiceFunc :: Behavior (Char -> Process)
          bProcChoiceFunc =
            flip ((flip (!!)) . (subtract (fromEnum '1')) . fromEnum) . fromJust <$>
            bProcChoiceMay
\end{code}
}

\defslide{ProcessorEProcChoice}{
\textit{eProcChoice} applies \textit{bProcChoiceFunc} on the stream of
\textit{eChooserChoice}.  The resulting stream is the \textit{Process}es chosen
by the user: either \textit{g1} or \textit{g2}.

\begin{code}
          eProcChoice :: Event Process
          eProcChoice = bProcChoiceFunc <@> eChooserChoice
\end{code}
}

\defslide{ProcessorBProcIsChoice}{
\textit{bProcIsChoice} is \textit{True} only if \textit{bProcChoiceMay}
contains the choice of \textit{g1} and \textit{g2}.  Otheriwse,
\textit{bProcIsChoice} is \textit{False}.

\begin{code}
          bProcIsChoice :: Behavior Bool
          bProcIsChoice = maybe False (const True) <$> bProcChoiceMay
\end{code}
}

\defslide{ProcessorEChooseMay}{
\textit{eChooseMay} allows us to look at the \textit{eKey} stream only when the
list of remaining \textit{Process}es is a \textit{GlobalChoice}.

\begin{code}
          eChooseMay :: Event Char
          eChooseMay = whenE bProcIsChoice (filterJust eKey)
\end{code}
}

\defslide{ProcessorEChooseMayNot}{
\textit{eChooseMayNot} allows us to look at the \textit{eKey} stream only when
the list of remaining \textit{Process}es is not a \textit{GlobalChoice}.

\begin{code}
          eChooseMayNot :: Event Char
          eChooseMayNot = whenE (not <$> bProcIsChoice) (filterJust eKey)
\end{code}
}

\defslide{ProcessorEChooseScopes}{
The partitioned scopes of \textit{eChooseMay} and \textit{eChooseMayNot}
ensures that further stream processing is similarly partitioned.
}

\defslide{ProcessorEStepper}{
\textit{eStepper} is the stream of \textit{eChooseMayNot} when
\textit{eChooseMayNot} has \textit{'s'} in its data stream.

\begin{code}
          eStepper :: Event Char
          eStepper = filterE (== 's') eChooseMayNot
\end{code}
}

\defslide{ProcessorEDigit}{
\textit{eDigit} is the stream of \textit{eChooseMay} when \textit{eChooseMay}
has any digits in its data stream.

\begin{code}
          eDigit :: Event Char
          eDigit = filterE isDigit eChooseMay
\end{code}
}

\defslide{ProcessorEChooserChoice}{
\textit{eChooserChoice} is the stream of \textit{'1'}s and \textit{'2'}s inside
\textit{eDigit}.

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
  \item \textit{unionWith} merges two incoming streams into one, without
  duplicates
  \item The two incoming streams, \textit{eProcChoice} and \textit{eStepper},
  have disjoint scopes.
  \item To show that \textit{bProc} is the list of remaining
  \textit{Process}es, \textit{processStep} needs to return the correct
  \textit{bProc}.
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
\textit{bStepCount} is the number of steps the user made so far.  It
accumulates an integer with initial value zero, and applies an increment
function \textit{(+ 1)} whenever \textit{bProc} has a \textit{Just} value, and
the user makes a step.  This means that, when we are done processing, the input
\textit{Process} \textit{p} is completely consumed, and \textit{bProc} will
have \textit{Nothing}.  In this case, the increment function will not be
applied.

\begin{code}
      (bStepCount :: Behavior Int) <-
        accumB
          0
          ((+ 1) <$ whenE ((maybe False (const True)) <$> bProc) eStepper)
\end{code}
}

\defslide{ProcessorEDone}{
\textit{eDone} indicates that \textit{bProc} is completely consumed.  Note the
similarities in implementation as compared to \textit{bStepCount}, except that
the data in \textit{eStepper} is converted to the unit value \textit{()} before
further processing.  This ensures that \textit{eDone} will have a type of
\textit{Event ()}.  We use the unit type because we only need an event to
indicate that processing is done.

\begin{code}
      let eDone :: Event ()
          eDone = whenE ((maybe True (const False)) <$> bProc) (() <$ eStepper)
\end{code}
}

\defslide{ProcessorNetworkDescription}{
\textit{networkDescription} returns a 4-tuple \textit{(eTrans, bProc,
eDone, bStepCount)} to be used by \textit{networkPrinter} and external modules
(like \textit{networkTransmit} in \textit{Frontend.lhs}).

\begin{code}
      return (eTrans, bProc, eDone, bStepCount)
\end{code}
}

\defslide{ProcessorNetworkPrinterDesc}{
\textit{networkPrinter} allows the processor to be run on the textual interface
defined in \textit{Interpreter.lhs}.  We print many things:
\begin{enumerate}
  \item Transmissions from \textit{eTrans},
  \item Changes in \textit{bProc}, which are first converted to
  \textit{GlobalProtocol}s by the \textit{mayProcessToGlobalProtocol} function,
  then projected to endpoint transmissions.
  \item Changes in step count,
  \item \textit{"Done!"} indicates that the processor completely consumed
  \textit{bProc}.
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
\textit{mayProcessToGlobalProtocol} returns a \textit{EGlobalProtocolEmp} if
its input is \textit{Nothing}.  Otherwise, it applies a fixpoint function on
the input: this fixpoint function is accessible from within itself as
\textit{r}.  We deconstruct \textit{Leaf} \textit{Process}es in the expected
manner.  We deconstruct the other \textit{Process}es, \textit{NodeS}s and
\textit{NodeC}s, by recursing on only the head of the contained lists.  If the
list is empty, then the fixpoint function returns \textit{EGlobalProtocolEmp}.

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
\textit{processStep} deconstructs a single level of \textit{Process}, to return
a tuple of \textit{Maybe GlobalTransmission} and \textit{Maybe Process}.

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
