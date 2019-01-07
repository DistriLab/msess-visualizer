{-
 - SECTION PRAGMAS
 -}
{-
 - SECTION MODULE
 -}
module Main where

{-
 - SECTION IMPORTS
 -}
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (nub)
import Graphics.Gloss
  ( Display(InWindow)
  , Picture
  , blank
  , pictures
  , rectangleSolid
  , rectangleWire
  , scale
  , text
  , translate
  , white
  )
import Graphics.Gloss.Data.Extent (Extent, centerCoordOfExtent, makeExtent)
import qualified Graphics.Gloss.Interface.IO.Game as Gloss (Event(EventKey))
import Graphics.Gloss.Interface.IO.Game (Key(Char), KeyState(Down), playIO)
import Parser (AnyExpr(AnyExpr), Expr, GlobalProtocol, extractFile)
import Reactive
  ( Process(Leaf)
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

wWidth = 320

wHeight = 240

main :: IO ()
main = do
  xs <- extractFile "test/reactive/example"
  let p = fmap head (parseContents xs)
      Just (Leaf g) = p -- TODO deconstruct p better
      picBase = drawParties (showParties g)
  picRef <- newIORef blank
  (eventHandler, fireEvent) <- newAddHandler
  network <-
    compile $ do
      glossEvent <- fromAddHandler eventHandler
      picture <-
        liftMoment $
        networkInput glossEvent >>= (\eKey -> networkProcessor eKey p) >>=
        networkOutput
      changes picture >>= reactimate' . fmap (fmap (writeIORef picRef))
      valueBLater picture >>= liftIO . writeIORef picRef
  actuate network
  playIO
    (InWindow "Frontend.hs" (wWidth, wHeight) (0, 0))
    white
    30
    ()
    (\() -> do
       modifyIORef' picRef (\pic -> pictures [picBase, pic])
       readIORef picRef)
    (\e () -> fireEvent e)
    (\_ () -> pure ())

{-
 - SECTION NETWORK
 -}
networkInput :: Event Gloss.Event -> Moment (Event Char)
networkInput glossEvent = return $ filterJust (mayKey <$> glossEvent)

networkOutput ::
     (Event (Maybe (Expr GlobalProtocol)), Behavior (Maybe Process), Event Char)
  -> Moment (Behavior Picture)
networkOutput (eTrans, bProc, eDone) = return $ pure blank

showParties :: Expr GlobalProtocol -> [String]
showParties = nub . map (un . AnyExpr) . partiesInGlobalProtocol

drawParties :: [String] -> Picture
drawParties ss = pictures $ map (uncurry $ drawParty w h) (zip extents ss)
  where
    extents = getPartiesExtents ss w h s
    -- Each charater has about 8 pixels of width
    -- Make width of all extents the width of the greatest extent
    w = 8 * ((maximum . map length) ss)
    h = 20
    s = 2

drawParty :: Int -> Int -> Extent -> String -> Picture
drawParty w h ex s = pictures $ map (translate xf yf) shapes
  where
    (x, y) = centerCoordOfExtent ex
    (xf, yf) = (fromIntegral x, fromIntegral y)
    (wf, hf) = (fromIntegral w, fromIntegral h)
    wHeightf = fromIntegral wHeight
    -- Define shapes
    drawnBox = rectangleWire wf hf
    drawnText = (scale 0.1 0.1 . translate (-240) (-50) . text) s
    drawLine =
      translate 0 (-hf / 2 - wHeightf) (rectangleSolid 2 (wHeightf * 2))
    shapes = [drawnBox, drawnText, drawLine]

-- All party extents in one line at the top
getPartiesExtents :: [String] -> Int -> Int -> Int -> [Extent]
getPartiesExtents ss w h s =
  [ makeExtent (h + yoffset) yoffset (x + w + xoffset) (x + xoffset)
  | x <- take num [0,(w + s) ..]
  ]
  where
    num = length ss
    xoffset = (-320)
    yoffset = 200

mayKey :: Gloss.Event -> Maybe Char
mayKey e =
  case e of
    Gloss.EventKey (Char c) Down _ p -> Just c
    otherwise -> Nothing
