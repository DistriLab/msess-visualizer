{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.IORef (newIORef, readIORef, writeIORef)
import Graphics.Gloss
  ( Display(InWindow)
  , Picture(Text, Translate)
  , blank
  , white
  )
import qualified Graphics.Gloss.Interface.IO.Game as Gloss (Event(EventKey))
import Graphics.Gloss.Interface.IO.Game
  ( Key(Char)
  , KeyState(Down)
  , playIO
  , scale
  )
import Parser (Expr, GlobalProtocol, extractFile)
import Reactive (Process, networkProcessor, parseContents)
import Reactive.Banana
  ( Behavior
  , Event
  , Moment
  , compile
  , filterJust
  , liftMoment
  , valueBLater
  )
import Reactive.Banana.Frameworks
  ( actuate
  , changes
  , fromAddHandler
  , liftIO
  , newAddHandler
  , reactimate'
  )

main :: IO ()
main = do
  picRef <- newIORef blank
  (eventHandler, fireEvent) <- newAddHandler
  network <-
    compile $ do
      glossEvent <- fromAddHandler eventHandler
      xs <- liftIO $ extractFile "test/reactive/example"
      picture <-
        liftMoment $
        networkInput glossEvent >>=
        (\eKey -> networkProcessor eKey (fmap head (parseContents xs))) >>=
        networkOutput
      changes picture >>= reactimate' . fmap (fmap (writeIORef picRef))
      valueBLater picture >>= liftIO . writeIORef picRef
  actuate network
  playIO
    (InWindow "Reactive-Banana Example" (320, 240) (800, 200))
    white
    30
    ()
    (\() -> readIORef picRef)
    (\e () -> fireEvent e)
    (\_ () -> pure ())

{-
 - SECTION NETWORK
 -}
networkInput :: Event Gloss.Event -> Moment (Event Char)
networkInput glossEvent = do
  return $ 's' <$ filterJust (isStepper <$> glossEvent)

networkOutput ::
     (Event (Maybe (Expr GlobalProtocol)), Behavior (Maybe Process), Event Char)
  -> Moment (Behavior Picture)
networkOutput (eOut, bProc, eDone) = do
  return $ (pure . Translate (-320) (120) . scale 0.2 0.2 . Text) "Hello World"

isStepper :: Gloss.Event -> Maybe ()
isStepper e =
  case e of
    Gloss.EventKey (Char 's') Down _ p -> Just ()
    otherwise -> Nothing
