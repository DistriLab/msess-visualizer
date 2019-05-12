\defslide{FrontendIntroduction}{
The frontend displays the textual results from \textit{Processor} as graphics.
We define graphical objects, then combine them according to the
\textit{Processor}.
}

%if False
\begin{code}
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
import Graphics.Gloss.Interface.IO.Game
  ( Key(Char, MouseButton)
  , KeyState(Down)
  , MouseButton(WheelDown, WheelUp)
  , playIO
  )
import Parser
  ( Boole(EBoole)
  , Channel(EChannel)
  , Formula(EFormulaExists)
  , GlobalProtocol(EGlobalProtocolTransmission)
  , GlobalProtocol
  , Heap(EHeapEmp)
  , Label(ELabel)
  , Pure(EPureBoole)
  , Role(ERole)
  , Role
  , VarFirst(EVarFirst)
  , extractFile
  )
import qualified Parser (Event(EEvent))
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

exXOffset = -(div wWidth 2)

exYOffset = (div wHeight 2) - exHeight

transmitStepCountSpace = 20

transmitHeadHeight = 10

transmitHeadLength = 10

transmitThickness = 2

transmitDescYOffset = 5

textScale = 0.1

numTransmitsOnScreenMax =
  div ((div wHeight 2) - exHeight) transmitStepCountSpace
\end{code}
%endif

%if False
\begin{code}
{-
 - SECTION MAIN
 -}
\end{code}
%endif

\defslide{FrontendDiagram}{
\begin{center}
\includegraphics[scale=0.225]{../ecce/plantuml/network.png}
\end{center}
}

\defslide{FrontendOverview}{
\begin{itemize}
  \item Extract file contents,
  \item Parsing contents to \textit{GlobalProtocol} \textit{g},
  \item Get parties involved in \textit{g},
  \item Map each party to an \textit{Extent}.
  \item Draw \textit{Transmit}s between \textit{Extent}s.
\end{itemize}

%if False
\begin{code}
main :: IO ()
main = do
  xs <- extractFile "test/processor/example"
  let p = fmap head (parseContents xs)
      Just (Leaf g) = p
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
\end{code}
%endif

%if False
\begin{code}
{-
 - SECTION TYPES
 -}
\end{code}
%endif

\textit{Transmit}s are tuples of sender x-coord, receiver x-coord, transmission
y-coord, and a description.

\begin{code}
type Transmit = (Float, Float, Float, String)
\end{code}
}

%if False
\begin{code}
{-
 - SECTION NETWORK
 -}
{-
 - SUBSECTION NETWORK DESCRIPTION
 -}
\end{code}
%endif

\defslide{FrontendNetworkDescription}{
\textit{networkDescription} displays a picture on the screen.  It runs the
Kleisli arrow \textit{(aPicture p extentsMap)} to get a picture, then monitors
changes in the picture and displays to the screen through a reference.

\begin{code}
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
\end{code}
}

\defslide{FrontendAPicture}{
Composition of many Kleisli arrows.

\begin{code}
aPicture p extentsMap =
  proc eGloss ->
  do eCharMay <- Kleisli networkInput -< eGloss
     bTransmits <- Kleisli
                     (networkProcessor p >=>
                        networkTransmit extentsMap >=> networkTransmitAccum)
                     -< eCharMay
     bScrollPos <- Kleisli (networkInputScroll >=> networkOutputScroll)
                     -< eGloss
     picture <- Kleisli networkDraw -< (bTransmits, bScrollPos)
     returnA -< picture
\end{code}
}

%if False
\begin{code}
{-
 - SUBSECTION NETWORK INPUTS
 -}
\end{code}
\begin{code}
networkInput :: Event Gloss.Event -> Moment (Event (Maybe Char))
networkInput eGloss = return $ mayKey <$> eGloss
\end{code}
%endif

%if False
\begin{code}
networkInputScroll :: Event Gloss.Event -> Moment (Event (Maybe MouseButton))
networkInputScroll eGloss = return $ mayScroll <$> eGloss
\end{code}
%endif

%if False
\begin{code}
{-
 - SUBSECTION NETWORK OUTPUTS
 -}
\end{code}
%endif

\defslide{FrontendNetworkDraw}{
\begin{itemize}
  \item Scroll position selects which transmissions drawn on screen,
  \item Pair each transmission with the y-coordinate of first transmission,
  which is used to offset all transmissions.
  \item Decompose each transmission into a \textit{transmitSRDesc}, which
  does not have a y-coordinate field.
\end{itemize}

\begin{code}
networkDraw :: (Behavior [Transmit], Behavior Int) -> Moment (Behavior Picture)
networkDraw (bTransmits, bScrollPos) = do
  return $
    pictures .
    map
      (\((sX, rX, y, desc), firstTransmitY) ->
         translate 0 (y - firstTransmitY) (transmitSRDesc sX rX desc)) .
    (\ts -> zip ts (repeat . getTransmitY . head $ ts)) <$>
    transmitsOnScreen
  where
    transmitsOnScreen =
      (\ts pos -> (take numTransmitsOnScreenMax . drop pos) ts) <$> bTransmits <*>
      bScrollPos
    getTransmitY (_, _, y, _) = y
\end{code}
}

%if False
\begin{code}
{-
 - SUBSECTION NETWORK PIPES
 -}
-- Treat sender and receiver as tuple
\end{code}
%endif

\defslide{FrontendNetworkTransmitType}{
\textit{networkTransmit} looks at the 4-tuple from \textit{Processor.lhs}, and
returns a stream of \textit{Transmit}s.  The coordinates of these
\textit{Transmit}s are determined by \textit{extentsMap}.

\begin{code}
networkTransmit ::
     [(String, Extent)]
  -> ( Event (Maybe GlobalProtocol)
     , Behavior (Maybe Process)
     , Event ()
     , Behavior Int)
  -> Moment (Event Transmit)
networkTransmit extentsMap (eTrans, bProc, eDone, bStepCount) = do
\end{code}
}

\defslide{FrontendNetworkTransmitETransJust}{
\textit{eTransJust} is identical to \textit{eTrans}, except when the value of
\textit{eTrans} is \textit{Nothing}.

\begin{code}
  let eTransJust = filterJust eTrans
\end{code}
}

\defslide{FrontendNetworkTransmitSREventDesc}{
\begin{itemize}
  \item Project into sender and receiver \textit{Event}s.
  \item Unparse the \textit{GlobalTransmission} to get the description.
\end{itemize}
Subsequent stream processings will preserve the transmission description in
\textit{srEventDesc}, because the transmission description does not need to be
processed.

\begin{code}
      srEventDesc =
        (\x ->
           ( let e = ev x
              in (head e, last e)
           , transToDesc x)) <$>
        eTransJust
\end{code}
}

%if False
\begin{code}
      -- [sender, receiver] in that order
\end{code}

\textit{srRoleDesc} converts each \textit{Event} in \textit{srEventDesc} into a
\textit{Role}.  Note the use of the \textit{mapTuple} function, which applies
the same function over both elements in a 2-tuple.  \textit{mapTuple} is used
again in later stream processings.

\begin{code}
      srRoleDesc =
        (\x -> ((mapTuple eventToRole . fst) x, snd x)) <$> srEventDesc
\end{code}

\textit{srExtentsDesc} takes each \textit{Role}, and constructs a
\textit{GlobalTransmission} with that role.  We do this because, once again,
\textit{un} must take a \textit{GlobalProtocol} as input.  Then, we look up the
unparsed string in \textit{extentsMap}, so that our 2-tuple now has a pair of
\textit{Extent}s.

\begin{code}
      srExtentsDesc =
        (\x ->
           ( (mapTuple
                ((\s -> lookup s extentsMap) .
                 un .
                 (\r ->
                    EGlobalProtocolTransmission
                      r
                      (ELabel 0)
                      (ERole "")
                      (EChannel "")
                      (EVarFirst 0) $
                    EFormulaExists [] EHeapEmp $ EPureBoole $ EBoole True)) .
              fst)
               x
           , snd x)) <$>
        srRoleDesc
        -- TODO probably lookup returns Nothing
\end{code}

\textit{srXDesc} converts each \textit{Extent} into the x-coordinate of their
centers.  The first x-coordinate is the sender's, and the second x-coordinate
is the receiver's.

\begin{code}
      srXDesc =
        (\x -> ((mapTuple centerOfExtent . fst) x, snd x)) <$> srExtentsDesc
\end{code}

Finally, \textit{srXYDesc} actually constructs \textit{Transmit}s.  Since we
already have the sender's x-coordinate, the receiver's x-coordinate, and the
transmission description, all that is left is to calculate the y-coordinate of
the transmission.  This is done by first applying a constant offset
\textit{exYOffset}, then a variable offset that depends on \textit{bStepCount}.

\begin{code}
      srXYDesc =
        ((\step ->
            \((sX, rX), desc) ->
              ( sX
              , rX
              , fromIntegral $ exYOffset + (-step * transmitStepCountSpace)
              -- negative because time increases downwards
              , desc)) <$>
         bStepCount) <@>
        srXDesc
  return srXYDesc
  where
    mapTuple = join (***)
\end{code}
%endif

%if False
\begin{code}
networkTransmitAccum :: Event Transmit -> Moment (Behavior [Transmit])
networkTransmitAccum eTransmit = accumB [] ((\a -> (++) [a]) <$> eTransmit)
\end{code}
%endif

%if False
\begin{code}
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
\end{code}
%endif

%if False
\begin{code}
{-
 - SUBSECTION NETWORK HELPERS
 -}
transToDesc :: GlobalProtocol -> String
transToDesc t = un t

eventToRole :: Parser.Event -> Role
eventToRole (Parser.EEvent p _) = p

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
\end{code}
%endif

%if False
\begin{code}
{-
 - SECTION SHAPES
 -}
-- Draws transmit pointing East at origin
-- transmitbody length, transmithead height, transmithead length, transmit thickness
\end{code}
%endif

\defslide{FrontendTransmit}{
\textit{transmit} defines a transmission as a pictorial arrow, without an
origin.

\begin{code}
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
\end{code}
}

%if False
\begin{code}
-- Draws transmit from sender to receiver with description
-- Translate transmits so that tail is at sender and head is at receiver
-- Description is always on the left of transmit
-- Flips transmits if needed
\end{code}
%endif

\defslide{FrontendTransmitSRDescDesc}{
\begin{enumerate}
  \item \textit{transmitSRDesc} defines a transmission as a pictorial arrow,
  \underline{with} an origin.  The x-coordinate of the origin is always the
  sender of the transmission's x-coordinate.
  \item The direction of the pictorial arrow depends on the x-coordinate of the
  receiver relative to the x-coordinate of the sender.
\end{enumerate}
}

\defslide{FrontendTransmitSRDescCode}{
\begin{code}
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
\end{code}
}

%if False
\begin{code}
drawText = scale textScale textScale . text
\end{code}
%endif

%if False
\begin{code}
{-
 - SECTION PARTIES
 -}
showParties :: GlobalProtocol -> [String]
showParties = nub . map show . partiesInGlobalProtocol
\end{code}
%endif

%if False
\begin{code}
-- Also return extents map
\end{code}
%endif

%if False
\begin{code}
drawParties :: [String] -> (Picture, [(String, Extent)])
drawParties ss =
  (pictures $ map (uncurry $ drawParty w exHeight) extentsMap, extentsMap)
    -- Each charater has about 8 pixels of width
    -- Make width of all extents the width of the greatest extent
  where
    w = exWidth * ((maximum . map length) ss)
    extentsMap = mappingPartyExtent ss
\end{code}
%endif

%if False
\textit{drawParty} draws each party as text in a box, centered on its
\textit{Extent}, and a vertical line starting from and centered on the
x-coordinate of the box, ending at the bottom of the screen.
\par

\begin{code}
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
\end{code}
%endif

%if False
\begin{code}
-- All party extents in one line at the top
\end{code}
%endif

%if False
\textit{getPartiesExtents} creates an \textit{Extent} for each party.  An
\textit{Extent} is a rectangular area.  The width and height of each
\textit{Extent}, and the spacing between two \textit{Extent}s, are given as
\textit{w}, \textit{h}, and \textit{s} respectively.

\begin{code}
getPartiesExtents :: [String] -> Int -> Int -> Int -> [Extent]
getPartiesExtents ss w h s =
  [ makeExtent (h + exYOffset) exYOffset (x + w + exXOffset) (x + exXOffset)
  | x <- take num [0,(w + s) ..]
  ]
  where
    num = length ss
\end{code}
%endif

%if False
\begin{code}
mappingPartyExtent :: [String] -> [(String, Extent)]
mappingPartyExtent ss = zip ss (getPartiesExtents ss exWidth exHeight exSpace)
\end{code}
%endif

\section{Frontend}
\slide{FrontendIntroduction}
\slide{FrontendDiagram}
\slide{FrontendOverview}
\slide{FrontendNetworkDescription}
\slide{FrontendAPicture}
\slide{FrontendNetworkDraw}
\slide{FrontendNetworkTransmitType}
\slide{FrontendNetworkTransmitETransJust}
\slide{FrontendNetworkTransmitSREventDesc}
\slide{FrontendTransmit}
\slide{FrontendTransmitSRDescDesc}
\slide{FrontendTransmitSRDescCode}
