{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows patterm match on GADT
{-# LANGUAGE RecursiveDo #-} -- Allows mdo
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns

{-
 - SECTION MODULE
 -}
module Reactive where

{-
 - SECTION IMPORTS
 -}
import Base (extractParse)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import Data.Either (rights)
import Data.Functor ((<$), (<$>))
import Data.List (intercalate, nub)
import Data.Maybe (fromJust)
import Interpreter (Output, mainHaskeline)
import Parser
  ( AnyExpr(AnyExpr)
  , Channel
  , Expr(EEvent, EGlobalProtocolChoice, EGlobalProtocolConcurrency,
     EGlobalProtocolEmp, EGlobalProtocolSequencing,
     EGlobalProtocolTransmission)
  , Expr
  , GlobalProtocol
  , Role
  , extractFile
  , parseGlobalProtocol
  )
import Projector (ev, projectGlobalToParty, projectPartyToEndpoint, tr)
import Reactive.Banana
  ( Behavior
  , Event
  , Moment
  , (<@)
  , (<@>)
  , compile
  , filterE
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
              fromAddHandler addKeyEvent >>=
              (\eKey -> networkDescription eKey restInputLine)
            actuate network
            eventLoop fireKey network))
  , ( "test"
    , \(_, _, restInputLine) ->
        (do (addKeyEvent, fireKey) <- newAddHandler
            network <-
              compile $
              fromAddHandler addKeyEvent >>=
              (\eKey ->
                 networkDescription eKey ("test/reactive/" ++ restInputLine))
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
  = Leaf (Expr GlobalProtocol)
  | NodeS [Process]
  | NodeC [Process]
  deriving (Show)

-- SECTION NETWORK
networkDescription :: Event Char -> FilePath -> MomentIO ()
networkDescription eKey filePath =
  (liftIO $ extractFile filePath) >>=
  (\xs -> liftMoment $ networkProcessor eKey (fmap head (parseContents xs))) >>=
  networkPrinter -- TODO Unmanual extract first parsed content

networkProcessor ::
     Event Char
  -> Maybe Process
  -> Moment ( Event (Maybe (Expr GlobalProtocol))
            , Behavior (Maybe Process)
            , Event Char)
networkProcessor eKey p
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
                  Just (NodeS (Leaf (EGlobalProtocolChoice g1 g2):_)) ->
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
          eChooseMay = whenE bProcIsChoice eKey
          eChooseMayNot :: Event Char
          eChooseMayNot = whenE (not <$> bProcIsChoice) eKey
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
      (eTrans :: Event (Maybe (Expr GlobalProtocol)), bProc :: Behavior (Maybe Process)) <-
        mapAccum p $
        unionWith
          const
          (const <$> ((processStep . Just) <$> eProcChoice))
          (const <$> (processStep <$> bProc <@ eStepper))
      -- SUBSECTION STEPPER STATE
      -- eDone: whether the debugger is done
      let eDone :: Event Char
          eDone = whenE ((maybe True (const False)) <$> bProc) eStepper
      return (eTrans, bProc, eDone)

networkPrinter ::
     (Event (Maybe (Expr GlobalProtocol)), Behavior (Maybe Process), Event Char)
  -> MomentIO ()
networkPrinter (eTrans, bProc, eDone) = do
  reactimate $
    maybe (return ()) (putStrLn . ("Transmission: " ++) . un . AnyExpr) <$>
    eTrans
  reactimate $ putStrLn "Done!" <$ eDone
  eProc <- changes bProc
  -- TODO find more efficient way of getting endpoint protocols
  reactimate' $
    fmap
      (putStrLn .
       intercalate "\n" .
       nub .
       map (un . AnyExpr) .
       (\g ->
          [ projectPartyToEndpoint (projectGlobalToParty g p) c
          | p <- partiesInGlobalProtocol g
          , c <- channelsInGlobalProtocol g
          ]) .
       mayProcessToGlobalProtocol) <$>
    eProc

partiesInGlobalProtocol :: Expr GlobalProtocol -> [Expr Role]
partiesInGlobalProtocol g = [p | EEvent p _ <- ev g]

channelsInGlobalProtocol :: Expr GlobalProtocol -> [Expr Channel]
channelsInGlobalProtocol g =
  [c | EGlobalProtocolTransmission _ _ _ c _ _ <- tr g]

mayProcessToGlobalProtocol :: Maybe Process -> Expr GlobalProtocol
mayProcessToGlobalProtocol =
  maybe
    EGlobalProtocolEmp
    (fix
       (\r p ->
          case p of
            NodeS [] -> EGlobalProtocolEmp -- TODO figure out how to skip NodeS []
            NodeC [] -> EGlobalProtocolEmp -- TODO figure out how to skip NodeC []
            NodeS (p:ps) -> r p
            NodeC (p:ps) -> r p
            Leaf g -> g))

parseContents :: Either [String] [String] -> Maybe [Process]
parseContents xs =
  either
    (\e -> error $ "Parse error: " ++ show e)
    (\xs ->
       let gs = map (extractParse parseGlobalProtocol) xs
        in if any (either (const True) (const False)) gs
             then Nothing
             else Just $ map Leaf (rights gs))
    xs

processStep :: Maybe Process -> (Maybe (Expr GlobalProtocol), Maybe Process)
processStep p =
  case p of
    Nothing -> (Nothing, Nothing)
    Just p ->
      case p of
        Leaf g ->
          case g of
            EGlobalProtocolConcurrency g1 g2 ->
              (Nothing, Just $ NodeC [Leaf g1, Leaf g2])
            EGlobalProtocolSequencing g1 g2 ->
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
