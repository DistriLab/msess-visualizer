{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (join)
import Data.IORef (newIORef, readIORef, writeIORef)
import Graphics.Gloss
  ( Display(InWindow)
  , Picture(Text)
  , blank
  , scale
  , translate
  , white
  )
import qualified Graphics.Gloss.Interface.IO.Game as Gloss (Event(EventKey))
import Graphics.Gloss.Interface.IO.Game (Key(Char), KeyState(Down), playIO)
import Parser (AnyExpr(AnyExpr), Expr, GlobalProtocol, extractFile)
import Reactive
  ( Process
  , networkProcessor
  , parseContents
  , partiesInGlobalProtocol
  )
import Reactive.Banana
  ( Behavior
  , Event
  , Moment
  , compile
  , filterJust
  , liftMoment
  , stepper
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
import Unparser (un)

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
  return $ filterJust (mayKey <$> glossEvent)

networkOutput ::
     (Event (Maybe (Expr GlobalProtocol)), Behavior (Maybe Process), Event Char)
  -> Moment (Behavior Picture)
networkOutput (eOut, bProc, eDone) = do
  bOutString <- stepper "" (showParties <$> filterJust eOut)
  let picture = drawParties <$> bOutString
  return picture

showParties :: Expr GlobalProtocol -> String
showParties = join . map (un . AnyExpr) . partiesInGlobalProtocol

drawParties :: String -> Picture
drawParties = translate (-320) (120) . scale 0.2 0.2 . Text

mayKey :: Gloss.Event -> Maybe Char
mayKey e =
  case e of
    Gloss.EventKey (Char c) Down _ p -> Just c
    otherwise -> Nothing
