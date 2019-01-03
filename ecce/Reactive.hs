{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows patterm match on GADT

{-
 - SECTION MODULE
 -}
module Reactive where

{-
 - SECTION IMPORTS
 -}
import Backend
  ( Expr(EGlobalProtocolChoice, EGlobalProtocolConcurrency,
     EGlobalProtocolSequencing)
  , Expr
  , GlobalProtocol
  , extractFile
  , parseGlobalProtocol
  )
import Base (extractParse)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import Data.Either (rights)
import Data.Functor ((<$), (<$>))
import Interpreter (Output, mainHaskeline)
import Reactive.Banana
  ( Event
  , (<@)
  , (<@>)
  , accumB
  , compile
  , filterE
  , filterJust
  , stepper
  )
import Reactive.Banana.Frameworks
  ( AddHandler
  , EventNetwork
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
            network <- compile (networkDescription addKeyEvent restInputLine)
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

-- Set up the program logic in terms of events and behaviors.
networkDescription :: AddHandler Char -> FilePath -> MomentIO ()
networkDescription addKeyEvent restInputLine = do
  eKey <- fromAddHandler addKeyEvent
  let eStepper = filterE (== 's') eKey
      eDigit :: Event Char
      eDigit = filterE isDigit eKey
  -- xs: contents of file
  xs <- liftIO $ extractFile restInputLine
  -- bOutputProc: tuple of two elements:
  --    (1) output string
  --    (2) process that the debugger currently has
  bOutputProc <-
    accumB ("", fmap head (parseContents xs)) $
    (flip $ const . uncurry (\_ -> processStep)) <$> (("", Nothing) <$ eStepper)
  let bOutput = fst <$> bOutputProc
      bProc = snd <$> bOutputProc
  eOutputChange <- changes bOutput
  -- eChooseMay:
  --    whether user may select a choice
  --    In this context, happens when bProc has EGlobalProtocolChoice as the 
  --    next Process
  -- eChooserChoice: choice selected by user, scoped to EGlobalPRotocolChoice
  -- TODO Note:
  --    `bProc <@ eStepper`: cannot filterE Future, hence use combinator for 
  --    Behavior and Event
  let eChooseMay :: Event (Maybe Process)
      eChooseMay =
        filterE
          (\x ->
             case x of
               Just (NodeS (Leaf (EGlobalProtocolChoice _ _):_)) -> True -- TODO assumed NodeS, not NodeC
               otherwise -> False) $
        bProc <@ eStepper
  -- bRunning:
  --    whether debugger is running
  --    not running when user choosing
  --    running when user chose
  bRunning <- accumB True $ (flip $ const id) <$> (False <$ eChooseMay)
  -- eRunning: whether the debugger is running
  -- eRunningNot: whether the debugger has stopped running
  -- eDone: whether the debugger is done
  let eRunning = bRunning <@ eStepper
      eRunningNot = filterE not $ bRunning <@ eStepper
      eDoneMay =
        (\m _ -> maybe (Just True) (const Nothing) m) <$> bProc <@> eStepper
      eDone = filterJust eDoneMay
  bChooserChoice <- stepper ' ' (filterE (`elem` "12") eDigit)
  let eChooserChoiceRunningNot = bChooserChoice <@ eRunningNot
  reactimate' $
    fmap
      (\x ->
         case x of
           "" -> return ()
           otherwise -> putStrLn x) <$>
    eOutputChange
  eChooserChoice <- changes bChooserChoice
  reactimate $ putStrLn "Done!" <$ eDone
  reactimate $ putStrLn . show <$> eRunningNot
  reactimate $ putStrLn . show <$> eChooserChoiceRunningNot

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

processStep :: Maybe Process -> (String, Maybe Process)
processStep Nothing = ("", Nothing)
processStep (Just (Leaf g)) =
  case g of
    EGlobalProtocolConcurrency g1 g2 -> ("", Just $ NodeC [Leaf g1, Leaf g2])
    EGlobalProtocolChoice g1 g2 -> ("", Just $ Leaf g2) -- TODO Unhardcode choice to g2
    EGlobalProtocolSequencing g1 g2 -> ("", Just $ NodeS [Leaf g1, Leaf g2])
    otherwise -> (show g, Nothing)
processStep (Just (NodeS [])) = ("", Nothing)
processStep (Just (NodeS (p:ps))) = (s', Just $ NodeS ps')
  where
    (s', p') = processStep (Just p)
    ps' = maybe ps (: ps) p'
processStep (Just (NodeC [])) = ("", Nothing)
processStep (Just (NodeC (p:ps))) = (s', Just $ NodeC ps')
  where
    (s', p') = processStep (Just p)
    ps' = maybe ps (: ps) p'
