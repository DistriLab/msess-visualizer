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
import Interpreter (Output, mainRegular)
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
main = mainRegular commandOutputs incommandOutput

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
-- Each process sequentially resolves transmissions
-- Each process can also have many concurrently running processes
data Process
  = ProcessLeaf (Expr GlobalProtocol)
  | ProcessNode [Process]
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
               else mapM_ putStrLn $
                    process (ProcessNode $ map ProcessLeaf (rights gs)))
      xs
  estepper <- fromAddHandler (addHandler esstepper)
  estep <- accumE 0 $ (+ 1) <$ estepper
  reactimate $ fmap print estep

process :: Process -> [String]
process p =
  let aux :: [String] -> Process -> [String]
      aux ss (ProcessLeaf g)
        -- trace ("leaf: " ++ show g) $
       =
        case g of
          EGlobalProtocolConcurrency g1 g2 ->
            aux
              ss
              (ProcessNode
                 [ProcessNode [ProcessLeaf g1], ProcessNode [ProcessLeaf g2]])
          EGlobalProtocolChoice g1 g2 -> aux ss (ProcessLeaf g2) -- TODO Unhardcode choice to g2
          EGlobalProtocolSequencing g1 g2 ->
            aux ss (ProcessNode [ProcessLeaf g1, ProcessLeaf g2])
          otherwise -> show g : ss
      aux ss (ProcessNode []) = ss
      aux ss (ProcessNode (p:ps))
        -- trace ("node: " ++ show (p : ps)) $
       = aux (join $ ss : aux [] p : []) (ProcessNode ps)
   in aux [] p
{-
    -- The state of the slot machine is captured in Behaviors.
        
    -- State: credits that the player has to play the game
    -- The  ecoin      event adds a coin to the credits
    -- The  edoesplay  event removes money
    -- The  ewin       event adds credits because the player has won
    (ecredits :: Event Money, bcredits :: Behavior Money)
        <- mapAccum 0 . fmap (\f x -> (f x,f x)) $ unions $
            [ addCredit    <$ ecoin
            , removeCredit <$ edoesplay
            , addWin       <$> ewin
            ]
    let
        -- functions that change the accumulated state
        addCredit     = (+1)
        removeCredit  = subtract 1
        addWin Double = (+5)
        addWin Triple = (+20)

        -- Event: does the player have enough money to play the game?
        emayplay :: Event Bool
        emayplay = (\credits _ -> credits > 0) <$> bcredits <@> eplay

        -- Event: player has enough coins and plays
        edoesplay :: Event ()
        edoesplay = () <$ filterE id  emayplay
        -- Event: event that fires when the player doesn't have enough money
        edenied   :: Event ()
        edenied   = () <$ filterE not emayplay

    -- State: random number generator
    (eroll :: Event Reels, bstdgen :: Behavior StdGen)
        -- accumulate the random number generator while rolling the reels
        <- mapAccum initialStdGen $ roll <$> edoesplay

    let
        -- roll the reels
        roll :: () -> StdGen -> (Reels, StdGen)
        roll () gen0 = ((z1,z2,z3),gen3)
            where
            random    = randomR(1,4)
            (z1,gen1) = random gen0
            (z2,gen2) = random gen1
            (z3,gen3) = random gen2
        
        -- Event: it's a win!
        ewin :: Event Win
        ewin = fmap fromJust $ filterE isJust $ fmap checkWin eroll
        checkWin (z1,z2,z3)
            | length (nub [z1,z2,z3]) == 1 = Just Triple
            | length (nub [z1,z2,z3]) == 2 = Just Double
            | otherwise                    = Nothing


    -- ecredits <- changes bcredits
    reactimate $ putStrLn . showCredit <$> ecredits
    reactimate $ putStrLn . showRoll   <$> eroll
    reactimate $ putStrLn . showWin    <$> ewin
    reactimate $ putStrLn "Not enough credits!" <$ edenied

-}
