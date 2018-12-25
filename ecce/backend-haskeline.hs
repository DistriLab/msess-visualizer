{-
 - SECTION IMPORTS
 -}
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )
import System.IO (IOMode(ReadMode), hClose, hGetContents, openFile)

{-
 - SECTION USER INTERFACE
 -}
main :: IO ()
main = do
  welcome
  runInputT defaultSettings interpreter

welcome :: IO ()
welcome = do
  mapM_ putStrLn $
    "Welcome!" :
    "Type at the prompt. Either:" :
    "1) Type out expression, or" : "2) Load file (load <file>)" : "" : []

interpreter :: InputT IO ()
interpreter = do
  mInputLine <- getInputLine "msess> "
  case mInputLine of
    Nothing -> outputStrLn "Quitting"
    Just inputLine -> (liftIO $ interpret inputLine) >> interpreter

interpret :: String -> IO ()
interpret inputLine = do
  inputLines <-
    if take loadStringLength inputLine == loadString
      then do
        handle <- openFile (drop loadStringLength inputLine) ReadMode
        contents <- hGetContents handle
        seq (hClose handle) (return $ lines contents)
      else do
        return [inputLine]
  let s = intercalate "\n" $ map show inputLines
   in putStrLn s
  where
    loadString = "load "
    loadStringLength = length loadString
