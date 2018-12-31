{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns

{-
 - SECTION IMPORTS
 -}
import Backend
  ( Expr(EGlobalProtocolChoice, EGlobalProtocolConcurrency,
     EGlobalProtocolSequencing)
  , Expr
  , GlobalProtocol
  , extractFile
  , extractParse
  , parseCommand
  , parseGlobalProtocol
  , parseRestInputLine
  , welcome
  )
import Control.Monad (join, when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft, rights)

-- import Debug.Trace (trace)
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
import Text.Parsec (ParseError)

{-
 - SECTION USER INTERFACE
 -}
main :: IO ()
main = do
  welcome
  interpreter

interpreter :: IO ()
interpreter = do
  putStr "ecce> "
  inputLine <- getLine
  Main.interpret inputLine
  interpreter

interpret :: String -> IO ()
interpret inputLine =
  case command of
    Nothing -> mapM_ putStrLn $ "Here are a list of commands:" : commands
    Just "help" -> mapM_ putStrLn $ "Here are a list of commands:" : commands
    Just "load" -> do
      sources <- newAddHandler
      network <- compile $ networkDescription sources restInputLine
      actuate network
      eventLoop sources network
    -- TODO fix double parsing
    -- TODO find better way to extract parsed expression than (Right .. =)
  where
    Right command = extractParse parseCommand inputLine
    Right restInputLine = extractParse parseRestInputLine inputLine

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
