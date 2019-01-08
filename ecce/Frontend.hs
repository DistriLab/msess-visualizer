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
  , rotate
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

{-
 - SECTION CONFIG
 -}
wWidth = 320

wHeight = 240

exWidth = 8

exHeight = 20

exSpace = 2

exXOffset = (-320)

exYOffset = 200

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
networkOutput (eTrans, bProc, eDone) = return $ pure $ arrow 100 50 50 2

mayKey :: Gloss.Event -> Maybe Char
mayKey e =
  case e of
    Gloss.EventKey (Char c) Down _ p -> Just c
    otherwise -> Nothing

{-
 - SECTION SHAPES
 -}
-- Draws arrow pointing East at origin
-- arrowbody length, arrowhead height, arrowhead length, arrow thickness
arrow :: Float -> Float -> Float -> Float -> Picture
arrow bl hh hl t = pictures [arrowBody, leftHead arrowHead, rightHead arrowHead]
  where
    angleHead = atan ((hh / 2) / hl)
    hypHead = hl / (cos angleHead)
    arrowBody = rectangleSolid bl t
    arrowHead = rectangleSolid hypHead t
    leftHead = translate ((bl - hl) / 2) (hh / 4) . rotate (degrees angleHead)
    rightHead =
      translate ((bl - hl) / 2) (-hh / 4) . rotate (degrees (-angleHead))
    degrees = (*) (180 / pi)

{-
 - SECTION PARTIES
 -}
showParties :: Expr GlobalProtocol -> [String]
showParties = nub . map (un . AnyExpr) . partiesInGlobalProtocol

drawParties :: [String] -> Picture
drawParties ss =
  pictures $ map (uncurry $ drawParty w exHeight) (mappingPartyExtent ss)
    -- Each charater has about 8 pixels of width
    -- Make width of all extents the width of the greatest extent
  where
    w = exWidth * ((maximum . map length) ss)

drawParty :: Int -> Int -> String -> Extent -> Picture
drawParty w h s ex = pictures $ map (translate xf yf) shapes
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
  [ makeExtent (h + exYOffset) exYOffset (x + w + exXOffset) (x + exXOffset)
  | x <- take num [0,(w + s) ..]
  ]
  where
    num = length ss

mappingPartyExtent :: [String] -> [(String, Extent)]
mappingPartyExtent ss = zip ss (getPartiesExtents ss exWidth exHeight exSpace)
