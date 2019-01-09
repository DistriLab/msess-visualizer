{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows patterm match on GADT

{-
 - SECTION MODULE
 -}
module Frontend where

{-
 - SECTION IMPORTS
 -}
import Control.Arrow ((***))
import Control.Monad (join)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (intercalate, nub)
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
import Graphics.Gloss.Interface.IO.Game
  ( Key(Char, MouseButton)
  , KeyState(Down)
  , MouseButton(WheelDown, WheelUp)
  , playIO
  )
import Parser
  ( AnyExpr(AnyExpr)
  , Expr(EEvent, EGlobalProtocolTransmission)
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
  , (<@>)
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

exSpace = 130

exXOffset = (-320)

exYOffset = 200

arrowStepCountSpace = 20

arrowHeadHeight = 10

arrowHeadLength = 10

arrowThickness = 2

{-
 - SECTION MAIN
 -}
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
      pictureScroll <-
        liftMoment $ networkInputScroll glossEvent >>= networkOutputScroll
      changes pictureScroll >>= reactimate' . fmap (fmap (writeIORef picRef))
      valueBLater pictureScroll >>= liftIO . writeIORef picRef
  actuate network
  playIO
    (InWindow "Frontend.hs" (wWidth, wHeight) (0, 0))
    white
    30
    ()
    (\() -> do
       pic <- readIORef picRef
       return $ pictures [picBase, pic])
    (\e () -> fireEvent e)
    (\_ () -> return ())

{-
 - SECTION NETWORK
 -}
networkInput :: Event Gloss.Event -> Moment (Event (Maybe Char))
networkInput glossEvent = return $ mayKey <$> glossEvent

networkInputScroll :: Event Gloss.Event -> Moment (Event (Maybe MouseButton))
networkInputScroll glossEvent = return $ mayScroll <$> glossEvent

-- Treat sender and receiver as tuple
networkOutput ::
     [(String, Extent)]
  -> ( Event (Maybe (Expr GlobalProtocol))
     , Behavior (Maybe Process)
     , Event Char
     , Behavior Int)
  -> Moment (Behavior Picture)
networkOutput extentsMap (eTrans, bProc, eDone, bStepCount) = do
  let eTransJust = filterJust eTrans
      srEventDesc =
        (\x ->
           ( let e = ev x
              in (head e, last e) -- TODO VERY UNSAFE
           , transToDesc x)) <$>
        eTransJust
      -- [sender, receiver] in that order
      srRoleDesc =
        (\x -> ((mapTuple eventToRole . fst) x, snd x)) <$> srEventDesc
      srExtentsDesc =
        (\x ->
           ( (mapTuple ((\s -> lookup s extentsMap) . un . AnyExpr) . fst) x
           , snd x)) <$>
        srRoleDesc -- TODO probably lookup returns Nothing
      srXDesc =
        (\x -> ((mapTuple centerOfExtent . fst) x, snd x)) <$> srExtentsDesc
      srXStepDesc =
        ((\step -> \((sX, rX), desc) -> (sX, rX, step, desc)) <$> bStepCount) <@>
        srXDesc
  picture <-
    accumB blank $
    (\(sX, rX, step, desc) ->
       (\pic ->
          pictures
            [ pic
            , translate
                0
                (fromIntegral $ exYOffset + (-step * arrowStepCountSpace)) -- negative because time increases downwards
                (arrowSRDesc sX rX desc)
            ])) <$>
    srXStepDesc
  return picture
  where
    mapTuple = join (***)

networkOutputScroll :: Event (Maybe MouseButton) -> Moment (Behavior Picture)
networkOutputScroll glossEvent = do
  bn <- accumB 0 ((+ 1) <$ filterJust glossEvent)
  let picture = (translate (-320) (120) . scale 0.2 0.2 . text . show) <$> bn
  return picture

transToDesc :: Expr GlobalProtocol -> String
transToDesc (EGlobalProtocolTransmission _ i _ c v f) =
  intercalate " " $ map un [AnyExpr i, AnyExpr c, AnyExpr v, AnyExpr f]

eventToRole :: Expr Parser.Event -> Expr Role
eventToRole (EEvent p _) = p

-- TODO srX :: (Maybe Extent, Maybe Extent) -> (Float, Float)
-- handle this problem badly by changing type of this function
-- and retruning garbage when input is Nothing
centerOfExtent :: Maybe Extent -> Float
centerOfExtent ex =
  case ex of
    Nothing -> 0
    Just ex -> (fromIntegral . fst . centerCoordOfExtent) ex

mayKey :: Gloss.Event -> Maybe Char
mayKey e =
  case e of
    Gloss.EventKey (Char c) Down _ p -> Just c
    otherwise -> Nothing

mayScroll :: Gloss.Event -> Maybe MouseButton
mayScroll e =
  case e of
    Gloss.EventKey (MouseButton WheelDown) Down _ p -> Just WheelDown
    Gloss.EventKey (MouseButton WheelUp) Down _ p -> Just WheelUp
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

-- Draws arrow from sender to receiver with description
-- Translate arrows so that tail is at sender and head is at receiver
-- Description is always on the left of arrow
-- Flips arrows if needed
arrowSRDesc :: Float -> Float -> String -> Picture
arrowSRDesc sX rX desc
  | sX < rX =
    translate (-(abs $ rX - distance / 2)) 0 $ pictures [arrowDesc, arrowSR]
  | sX > rX =
    (translate (-(abs $ sX - distance / 2)) 0) $
    pictures [arrowDesc, rotate 180 arrowSR]
  | otherwise = error "arrowsSR: sX and rX are too close"
  where
    distance = abs $ sX - rX
    arrowSR = arrow distance arrowHeadHeight arrowHeadLength arrowThickness
    arrowDesc = (translate (-distance / 2) 5 . drawText) desc

drawText = scale 0.1 0.1 . text

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
    drawLine =
      translate 0 (-hf / 2 - wHeightf) (rectangleSolid 2 (wHeightf * 2))
    shapes = [drawBox, (translate (-wf / 3) (-hf / 4) . drawText) s, drawLine]

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
