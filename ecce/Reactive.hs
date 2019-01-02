{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-}
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
import Control.Monad (join, when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft, rights)
import Debug.Trace (trace)
import Interpreter (Output, mainHaskeline)
import Reactive.Banana (accumE, compile)
import Reactive.Banana.Frameworks
  ( AddHandler
  , EventNetwork
  , MomentIO
  , actuate
  , fromAddHandler
  , newAddHandler
  , reactimate
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
        (do sources <- newAddHandler
            network <- compile $ networkDescription sources restInputLine
            actuate network
            eventLoop sources network))
  ]

incommandOutput :: Output
incommandOutput =
  \(_, commands, _) ->
    mapM_ putStrLn $ "Here are a list of commands:" : commands

-- Read commands and fire corresponding events 
eventLoop :: EventSource () -> EventNetwork -> IO ()
eventLoop esstepper network = loop
  where
    loop = do
      putStr "> "
      c <- getChar
      case c of
        's' -> fire esstepper ()
        'q' -> putStrLn "" >> return ()
        otherwise -> putStrLn " - unknown character"
      when (c /= 'q') loop

commands :: [String]
commands = "help" : "load" : []

{-----------------------------------------------------------------------------
    Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

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
networkDescription :: EventSource () -> FilePath -> MomentIO ()
networkDescription esstepper restInputLine = do
  xs <- liftIO $ extractFile restInputLine
  liftIO $
    either
      (mapM_ putStrLn)
      (\xs ->
         let gs = map (extractParse parseGlobalProtocol) xs
          in if any isLeft gs
               then putStrLn "some error"
               else mapM_ (mapM_ putStrLn . process . Leaf) (rights gs))
      xs
  estepper <- fromAddHandler (addHandler esstepper)
  estep <- accumE 0 $ (+ 1) <$ estepper
  reactimate $ fmap print estep

process :: Process -> [String]
process ps =
  let aux :: [String] -> Maybe [Process] -> [String]
      aux ss Nothing = ss
      aux ss (Just (p:ps)) = aux (ss ++ [s']) (Just ps')
        where
          (s', p') = processStep p
          ps' =
            case p' of
              Nothing -> ps
              Just x -> x : ps
      aux ss (Just _) = ss
   in aux [] (Just [ps])

processStep :: Process -> (String, Maybe Process)
processStep p =
  let aux :: String -> Process -> (String, Maybe Process)
      aux s (Leaf g) =
        case g of
          EGlobalProtocolConcurrency g1 g2 ->
            (s, Just $ NodeC [Leaf g1, Leaf g2])
          EGlobalProtocolChoice g1 g2 -> (s, Just $ Leaf g2) -- TODO Unhardcode choice to g2
          EGlobalProtocolSequencing g1 g2 ->
            (s, Just $ NodeS [Leaf g1, Leaf g2])
          otherwise -> (s ++ show g, Nothing)
      aux s (NodeS []) = (s, Nothing)
      aux s (NodeS (p:ps)) = aux s' (NodeS ps')
        where
          (s', p') = aux s p
          ps' =
            case p' of
              Nothing -> ps
              Just x -> x : ps
      aux s (NodeC []) = (s, Nothing)
      aux s (NodeC (p:ps)) = aux s' (NodeC ps')
        where
          (s', p') = aux s p
          ps' =
            case p' of
              Nothing -> ps
              Just x -> x : ps
   in aux "" p
