{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows patterm match on GADT

{-
 - SECTION MODULE
 -}
module Main where

{-
 - SECTION IMPORTS
 -}
import Control.Arrow ((***))
import Control.Monad (join)
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
import Parser
  ( AnyExpr(AnyExpr)
  , Expr(EEvent)
  , GlobalProtocol
  , Role
  , extractFile
  )
import qualified Parser (Event)
import Projector (ev)
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
  , (<$>)
  , accumB
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
      (picBase, extentsMap) = drawParties (showParties g)
  picRef <- newIORef blank
  (eventHandler, fireEvent) <- newAddHandler
  network <-
    compile $ do
      glossEvent <- fromAddHandler eventHandler
      picture <-
        liftMoment $
        networkInput glossEvent >>= (\eKey -> networkProcessor eKey p) >>=
        networkOutput extentsMap
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

-- Treat sender and receiver as tuple
networkOutput ::
     [(String, Extent)]
  -> (Event (Maybe (Expr GlobalProtocol)), Behavior (Maybe Process), Event Char)
  -> Moment (Behavior Picture)
networkOutput extentsMap (eTrans, bProc, eDone) = do
  let eTransEvents = fmap ev <$> eTrans
      -- [sender, receiver] in that order
      srEvent = filterJust eTransEvents
      srRole = (mapTuple eventToRole . (\x -> (head x, last x))) <$> srEvent -- TODO VERY UNSAFE
      srExtents =
        (mapTuple ((\s -> lookup s extentsMap) . un . AnyExpr)) <$> srRole
      srX = mapTuple centerOfExtent <$> srExtents
  picture <-
    accumB blank $
    (\(sX, rX) ->
       (\pic -> pictures [pic, translate sX 0 (arrow (abs $ sX - rX) 10 10 2)])) <$>
    srX
  return picture
  where
    mapTuple = join (***)

eventToRole :: Expr Parser.Event -> Expr Role
eventToRole (EEvent p _) = p

-- TODO srX :: (Maybe Extent, Maybe Extent) -> (Float, Float)
-- handle this problem badly by changing type of this function
-- and retruning garbage when input is Nothing
centerOfExtent :: Maybe Extent -> Float
centerOfExtent ex =
  case ex of
    Nothing -> 0
    Just ex ->
      (uncurry (/) .
       (\(x, y) -> (fromIntegral x, fromIntegral y)) . centerCoordOfExtent)
        ex

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

-- Also return extents map
drawParties :: [String] -> (Picture, [(String, Extent)])
drawParties ss =
  (pictures $ map (uncurry $ drawParty w exHeight) extentsMap, extentsMap)
    -- Each charater has about 8 pixels of width
    -- Make width of all extents the width of the greatest extent
  where
    w = exWidth * ((maximum . map length) ss)
    extentsMap = mappingPartyExtent ss

drawParty :: Int -> Int -> String -> Extent -> Picture
drawParty w h s ex = pictures $ map (translate xf yf) shapes
  where
    (x, y) = centerCoordOfExtent ex
    (xf, yf) = (fromIntegral x, fromIntegral y)
    (wf, hf) = (fromIntegral w, fromIntegral h)
    wHeightf = fromIntegral wHeight
    -- Define shapes
    drawBox = rectangleWire wf hf
    drawText = (scale 0.1 0.1 . translate (-240) (-50) . text) s
    drawLine =
      translate 0 (-hf / 2 - wHeightf) (rectangleSolid 2 (wHeightf * 2))
    shapes = [drawBox, drawText, drawLine]

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
