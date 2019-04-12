{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows patterm match on GADT
{-# LANGUAGE Arrows #-} -- Allows arrow notation

{-
 - SECTION MODULE
 -}
module Frontend where

{-
 - SECTION IMPORTS
 -}
import Control.Arrow (Kleisli(Kleisli), (***), returnA, runKleisli)
import Control.Monad ((>=>), join)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
import Processor
  ( Process(Leaf)
  , networkProcessor
  , parseContents
  , partiesInGlobalProtocol
  )
import Projector (ev)
import Reactive.Banana
  ( Behavior
  , Event
  , Moment
  , (<$)
  , (<$>)
  , (<*>)
  , (<@>)
  , accumB
  , compile
  , filterJust
  , liftMoment
  , valueBLater
  )
import Reactive.Banana.Frameworks
  ( MomentIO
  , actuate
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
fps = 30

wWidth = 640

wHeight = 480

exWidth = 8

exHeight = 20

exSpace = 130

exXOffset = (-320)

exYOffset = 200

transmitStepCountSpace = 20

transmitHeadHeight = 10

transmitHeadLength = 10

transmitThickness = 2

transmitDescYOffset = 5

textScale = 0.1

{-
 - SECTION MAIN
 -}
main :: IO ()
main = do
  xs <- extractFile "test/processor/example"
  let p = fmap head (parseContents xs)
      Just (Leaf g) = p -- TODO deconstruct p better
      (picBase, extentsMap) = drawParties (showParties g)
  picRef <- newIORef blank
  (eventHandler, fireEvent) <- newAddHandler
  network <-
    compile $
    fromAddHandler eventHandler >>= networkDescription p picRef extentsMap
  actuate network
  playIO
    (InWindow "Frontend.hs" (wWidth, wHeight) (0, 0))
    white
    fps
    ()
    (\() -> do
       pic <- readIORef picRef
       return $ pictures [picBase, pic])
    (\e () -> fireEvent e)
    (\_ () -> return ())

{-
 - SECTION TYPES
 -}
type Transmit = (Float, Float, Float, String)

{-
 - SECTION NETWORK
 -}
{-
 - SUBSECTION NETWORK DESCRIPTION
 -}
networkDescription ::
     Maybe Process
  -> IORef Picture
  -> [(String, Extent)]
  -> Event Gloss.Event
  -> MomentIO ()
networkDescription p picRef extentsMap eGloss = do
  picture <- liftMoment $ runKleisli (aPicture p extentsMap) eGloss
  changes picture >>= reactimate' . fmap (fmap (writeIORef picRef))
  valueBLater picture >>= liftIO . writeIORef picRef

aPicture p extentsMap =
  proc eGloss ->
  do eCharMay <- Kleisli networkInput -< eGloss
     bTransmits <- Kleisli
                     (networkProcessor p >=>
                        networkTransmit extentsMap >=> networkTransmitAccum)
                     -< eCharMay
     bScrollPos <- Kleisli (networkInputScroll >=> networkOutputScroll)
                     -< eGloss
     picture <- Kleisli networkDraw -<
                  (bTransmits, bScrollPos, eCharMay)
     returnA -< picture

{-
 - SUBSECTION NETWORK INPUTS
 -}
networkInput :: Event Gloss.Event -> Moment (Event (Maybe Char))
networkInput eGloss = return $ mayKey <$> eGloss

networkInputScroll :: Event Gloss.Event -> Moment (Event (Maybe MouseButton))
networkInputScroll eGloss = return $ mayScroll <$> eGloss

{-
 - SUBSECTION NETWORK OUTPUTS
 -}
networkDraw ::
     (Behavior [Transmit], Behavior Int, Event (Maybe Char))
  -> Moment (Behavior Picture)
networkDraw (bTransmits, bScrollPos, eKey) = do
  bScrollPosAuto <- accumB 0 ((+ 1) <$ filterJust eKey)
  let bScrollPosCombined = (+) <$> bScrollPosAuto <*> bScrollPos
  return $
    (pictures .
     map (\(sX, rX, y, desc) -> translate 0 y (transmitSRDesc sX rX desc))) <$>
    ((\ts pos -> drop (length ts - pos) ts) <$> bTransmits <*>
     bScrollPosCombined)

{-
 - SUBSECTION NETWORK PIPES
 -}
-- Treat sender and receiver as tuple
networkTransmit ::
     [(String, Extent)]
  -> ( Event (Maybe (Expr GlobalProtocol))
     , Behavior (Maybe Process)
     , Event ()
     , Behavior Int)
  -> Moment (Event Transmit)
networkTransmit extentsMap (eTrans, bProc, eDone, bStepCount) = do
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
      srXYDesc =
        ((\step ->
            \((sX, rX), desc) ->
              ( sX
              , rX
              , fromIntegral $ exYOffset + (-step * transmitStepCountSpace) -- negative because time increases downwards
              , desc)) <$>
         bStepCount) <@>
        srXDesc
  return srXYDesc
  where
    mapTuple = join (***)

networkTransmitAccum :: Event Transmit -> Moment (Behavior [Transmit])
networkTransmitAccum eTransmit = accumB [] ((\a -> (++) [a]) <$> eTransmit)

networkOutputScroll :: Event (Maybe MouseButton) -> Moment (Behavior Int)
networkOutputScroll eMouse =
  accumB
    0
    (maybe
       id
       (\x ->
          case x of
            WheelUp -> (+ 1)
            WheelDown -> subtract 1) <$>
     eMouse)

{-
 - SUBSECTION NETWORK HELPERS
 -}
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
-- Draws transmit pointing East at origin
-- transmitbody length, transmithead height, transmithead length, transmit thickness
transmit :: Float -> Float -> Float -> Float -> Picture
transmit bl hh hl t =
  pictures [transmitBody, leftHead transmitHead, rightHead transmitHead]
  where
    angleHead = atan ((hh / 2) / hl)
    hypHead = hl / (cos angleHead)
    transmitBody = rectangleSolid bl t
    transmitHead = rectangleSolid hypHead t
    leftHead = translate ((bl - hl) / 2) (hh / 4) . rotate (degrees angleHead)
    rightHead =
      translate ((bl - hl) / 2) (-hh / 4) . rotate (degrees (-angleHead))
    degrees = (*) (180 / pi)

-- Draws transmit from sender to receiver with description
-- Translate transmits so that tail is at sender and head is at receiver
-- Description is always on the left of transmit
-- Flips transmits if needed
transmitSRDesc :: Float -> Float -> String -> Picture
transmitSRDesc sX rX desc
  | sX < rX =
    translate (-(abs $ rX - distance / 2)) 0 $
    pictures [transmitDesc, transmitSR]
  | sX > rX =
    (translate (-(abs $ sX - distance / 2)) 0) $
    pictures [transmitDesc, rotate 180 transmitSR]
  | otherwise = error "transmitsSR: sX and rX are too close"
  where
    distance = abs $ sX - rX
    transmitSR =
      transmit distance transmitHeadHeight transmitHeadLength transmitThickness
    transmitDesc =
      (translate (-distance / 2) transmitDescYOffset . drawText) desc

drawText = scale textScale textScale . text

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
