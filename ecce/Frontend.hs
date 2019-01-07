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
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import Graphics.Gloss
  ( Display(InWindow)
  , Picture
  , blank
  , pictures
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

width = 320

height = 240

main :: IO ()
main = do
  picRef <- newIORef blank
  (eventHandler, fireEvent) <- newAddHandler
  network <-
    compile $ do
      glossEvent <- fromAddHandler eventHandler
      xs <- liftIO $ extractFile "test/reactive/example"
      let p = fmap head (parseContents xs)
      picture <-
        liftMoment $
        networkInput glossEvent >>= (\eKey -> networkProcessor eKey p) >>=
        networkOutput p
      changes picture >>= reactimate' . fmap (fmap (writeIORef picRef))
      valueBLater picture >>= liftIO . writeIORef picRef
  actuate network
  playIO
    (InWindow "Frontend.hs" (width, height) (0, 0))
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
networkInput glossEvent = return $ filterJust (mayKey <$> glossEvent)

networkOutput ::
     Maybe Process
  -> (Event (Maybe (Expr GlobalProtocol)), Behavior (Maybe Process), Event Char)
  -> Moment (Behavior Picture)
networkOutput (Just (Leaf g)) (eOut, bProc, eDone) -- TODO deconstruct p better
 = do
  bOutString <- stepper [] (showParties g <$ eOut)
  let picture = drawParties <$> bOutString
  return picture

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
drawParty w h ex s = bg <> fg
  where
    (x, y) = centerCoordOfExtent ex
    (xf, yf) = (fromIntegral x, fromIntegral y)
    (wf, hf) = (fromIntegral w, fromIntegral h)
    bg = translate xf yf (rectangleWire wf hf)
    fg = (translate xf yf . scale 0.1 0.1 . translate (-240) (-50) . text) s

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
