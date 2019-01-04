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
  ( Behavior
  , Event
  , (<@)
  , (<@>)
  , accumB
  , compile
  , filterE
  , filterJust
  , mapAccum
  , stepper
  , unionWith
  , whenE
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

-- SECTION NETWORK
networkDescription :: AddHandler Char -> FilePath -> MomentIO ()
networkDescription addKeyEvent restInputLine =
  mdo eKey <- fromAddHandler addKeyEvent
      -- SUBSECTION USER INPUT
      -- eChooseMay:
      --    only fires if user may choose
      -- eChooserChoice: choice selected by user
      let bProcChoice :: Behavior Bool
          bProcChoice =
            ((\x ->
                case x of
                  Just (NodeS (Leaf (EGlobalProtocolChoice _ _):_)) -> True -- TODO assumed NodeS, not NodeC
                  otherwise -> False) <$>
             bProc)
          eChooseMay :: Event Char
          eChooseMay = whenE bProcChoice eKey
          eChooseMayNot :: Event Char
          eChooseMayNot = whenE (not <$> bProcChoice) eKey
          eStepper :: Event Char
          eStepper = filterE (== 's') eChooseMayNot
          eDigit :: Event Char
          eDigit = filterE isDigit eChooseMay
          eChooserChoice :: Event Char
          eChooserChoice = filterE (`elem` "12") eDigit
      -- SUBSECTION STEPPER
      -- xs: contents of file
      -- (eOutput, bProc): tuple of two elements:
      --    (1) output string
      --    (2) process that the debugger currently has
      --    processStep:
      --        Ignore the accumulated bProc
      --        Take in the new bProc
      xs <- liftIO $ extractFile restInputLine
      (eOutput :: Event String, bProc :: Behavior (Maybe Process)) <-
        mapAccum (fmap head (parseContents xs)) $
        const <$> (processStep <$> bProc <@ eStepper)
      -- SUBSECTION STEPPER STATE
      -- eDone: whether the debugger is done
      let eDone :: Event Char
          eDone = whenE ((maybe True (const False)) <$> bProc) eStepper
      reactimate $
        (\x ->
           case x of
             "" -> return ()
             otherwise -> putStrLn x) <$>
        eOutput
      reactimate $ putStrLn "Done!" <$ eDone
      reactimate $ putStrLn . (++ " eChooseMay") . show <$> eChooseMay
      reactimate $ putStrLn . (++ " eChooserChoice") . show <$> eChooserChoice

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
