\documentclass{article}
%include polycode.fmt
\begin{document}
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
-- A process is either a single GlobalProtocol, or
-- a list of other processes.
-- A sequential process resolves processes in order:
-- resolve the head process, then process the tails.
-- A concurrent process resolves processes without a defined order.
data Process
  = Leaf GlobalProtocol
  | NodeS [Process]
  | NodeC [Process]
  deriving (Show)

-- SECTION NETWORK
networkDescription :: FilePath -> Event Char -> MomentIO ()
networkDescription filePath eKey =
  (liftIO $ extractFile filePath) >>=
  (\xs ->
     liftMoment $
     networkProcessor (fmap head (parseContents xs)) (Just <$> eKey)) >>=
  networkPrinter -- TODO Unmanual extract first parsed content

networkProcessor ::
     Maybe Process
  -> Event (Maybe Char)
  -> Moment ( Event (Maybe GlobalProtocol)
            , Behavior (Maybe Process)
            , Event ()
            , Behavior Int)
networkProcessor p eKey
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
 =
  mdo let bProcChoiceMay :: Behavior (Maybe [Process])
          bProcChoiceMay =
            ((\x ->
                case x of
                  Just (NodeS (Leaf (EOpGlobalProtocolBinary g1 EGlobalProtocolChoice g2):_)) ->
                    Just [Leaf g1, Leaf g2] -- TODO assumed NodeS, not NodeC
                  otherwise -> Nothing) <$>
             bProc)
          bProcChoiceFunc :: Behavior (Char -> Process)
          bProcChoiceFunc =
            flip ((flip (!!)) . (subtract (fromEnum '1')) . fromEnum) . fromJust <$>
            bProcChoiceMay
          eProcChoice :: Event Process
          eProcChoice = bProcChoiceFunc <@> eChooserChoice
          bProcIsChoice :: Behavior Bool
          bProcIsChoice = maybe False (const True) <$> bProcChoiceMay
          eChooseMay :: Event Char
          eChooseMay = whenE bProcIsChoice (filterJust eKey)
          eChooseMayNot :: Event Char
          eChooseMayNot = whenE (not <$> bProcIsChoice) (filterJust eKey)
          eStepper :: Event Char
          eStepper = filterE (== 's') eChooseMayNot
          eDigit :: Event Char
          eDigit = filterE isDigit eChooseMay
          eChooserChoice :: Event Char
          eChooserChoice = filterE (`elem` "12") eDigit
      -- SUBSECTION STEPPER
      -- xs: contents of file
      -- (eTrans, bProc): tuple of two elements:
      --    (1) global transmission that the debugger output
      --    (2) process that the debugger currently has
      --    processStep:
      --        Ignore the accumulated bProc
      --        Take in the new bProc
      (eTrans :: Event (Maybe GlobalProtocol), bProc :: Behavior (Maybe Process)) <-
        mapAccum p $
        unionWith
          const
          (const <$> ((processStep . Just) <$> eProcChoice))
          (const <$> (processStep <$> bProc <@ eStepper))
      -- SUBSECTION STEPPER STATE
      -- bStepCount: number of eSteppers fired
      -- eDone: whether the debugger is done
      (bStepCount :: Behavior Int) <-
        accumB
          0
          ((+ 1) <$ whenE ((maybe False (const True)) <$> bProc) eStepper)
      let eDone :: Event ()
          eDone = whenE ((maybe True (const False)) <$> bProc) (() <$ eStepper)
      return (eTrans, bProc, eDone, bStepCount)

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
  -- TODO find more efficient way of getting endpoint protocols
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

mayProcessToGlobalProtocol :: Maybe Process -> GlobalProtocol
mayProcessToGlobalProtocol =
  maybe
    EGlobalProtocolEmp
    (fix
       (\r p ->
          case p of
            NodeS [] -> EGlobalProtocolEmp -- TODO how to skip NodeS []; maybe normalize emp;G to G
            NodeC [] -> EGlobalProtocolEmp -- TODO how to skip NodeC []; maybe normalize emp*G to G
            NodeS (p:ps) -> r p
            NodeC (p:ps) -> r p
            Leaf g -> g))

parseContents :: Either [String] [String] -> Maybe [Process]
parseContents xs =
  either
    (\e -> error $ "Parse error: " ++ show e)
    (\xs ->
       let gs = head $ map (extractParse parseGlobalProtocol) xs
        in Just $ map Leaf gs)
    xs

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
\end{document}
