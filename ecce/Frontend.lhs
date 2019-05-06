\subsection{Frontend}

This section defines the graphical hook to \textit{Processor}, which means it
displays the textual results from \textit{Processor} as graphics.  We first
define a bunch of graphical objects, like \textit{Extent}s and pictorial
arrows, then combine these objects according to the results from
\textit{Processor}.
\par
We chose the graphical application \textit{gloss}, because of the simplicity of 
its usage, which in turn simplifies explanation.

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

\textit{Frontend.lhs} has the only significant \textit{main} implementation,
since the interface is not textual, but graphical.  After extracting the
contents of a file, parsing its contents, and extracting the
\textit{GlobalProtocol} \textit{g}, we pass \textit{g} to \textit{showParties}
in order to get a list of parties involved in the protocol.  An overview can be
found in Fig. TODO.

\begin{center}
\includegraphics[scale=0.5]{../ecce/plantuml/network.png}
\end{center}

\textit{drawParties} defines the base picture and the mapping between each
party and the \textit{Extent}s defined in the package \textit{gloss}.  We
create a reference to a blank image, then compile a \textit{gloss} network,
binding an \textit{eventHandler} to the network.

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
\end{code}
%if False
\begin{code}
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

\textit{Transmit}s are tuples of sender x-coordinate, receiver x-coordinate,
transmission y-coordinate, and a transmission description.

\begin{code}
type Transmit = (Float, Float, Float, String)
\end{code}

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

The picture arrow, \textit{aPicture}, is a composition of many Kleisli arrows.
\textit{eGloss} is an event defined by \textit{gloss}.  \textit{networkInput}
extracts the \textit{Char} from \textit{eGloss}, then passes that \textit{Char}
into \textit{networkProcessor} defined in \textit{Processor.lhs}.
\par
Similar unpacking of scroll events is done for \textit{networkInputScroll}, and
\textit{networkOutputScroll} accumulates the scroll state as an
\textit{Integer}.
\par
Note the usage of the fish operator \textit{(>=>)}, which has the type:
\textit{(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c}. That is, it
composes two monadic functions with the same type as the monadic
\textit{return}. Providing a input to this composed function will yield a
monad.
\par
In this case, \textit{eCharMay} is input to the result of three composed
Kleisli arrows, and yields a \textit{Behavior} monad, captured by the variable
\textit{bTransmits}.

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
%if False
\begin{code}
{-
 - SUBSECTION NETWORK INPUTS
 -}
\end{code}
%endif
\begin{code}
networkInput :: Event Gloss.Event -> Moment (Event (Maybe Char))
networkInput eGloss = return $ mayKey <$> eGloss
\end{code}

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

\textit{networkDraw} takes the history of transmissions \textit{bTransmits},
and the current scroll position \textit{bScrollPos}, and returns a picture.
The current scroll position determines which transmissions from the
transmissions history is drawn on screen.  This is done by applying a function
on the product of \textit{bTransmits} and \textit{bScrollPos}, which selects a
contiguous range of indexes, [\textit{bScrollPos}, \textit{bScrollPos +
numTransmitsOnScreenMax}).  This selected list of transmissions is stored in
\textit{transmitsOnScreen}.
\par
Then, we wrap each transmission in a 2-tuple.  The first element is the
transmission, and the second element is the y-coordinate of the first
transmission, \textit{firstTransmitY}.  Then, we decompose each transmission
into a \textit{transmitSRDesc}, forgetting about the y-coordinate of that
transmission.  We forget the y-coordinate because we have already used that
information to translate the \textit{transmitSRDesc} by its own y-coordinate.
The \textit{firstTransmitY} is used to offset all transmissions by
\textit{firstTransmitY} pixels.

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

%if False
\begin{code}
{-
 - SUBSECTION NETWORK PIPES
 -}
-- Treat sender and receiver as tuple
\end{code}
%endif

\textit{networkTransmit} looks at the 4-tuple \textit{(eTrans, bProc,
eDone, bStepCount)} returned by \textit{networkDescription} in
\textit{Processor.lhs}, and returns a stream of \textit{Transmit}s.  These
\textit{Transmit}s have coordinates determined by \textit{extentsMap}.
\par
\textit{networkTransmit} uses \textit{Behavior}s and \textit{Event}s from
\textit{reactive-banana}.  These \textit{reactive-banana} \textit{Event}s are
not to be confused with \textit{Event}s from \textit{gloss}.
\textit{networkTransmit} performs stream processing.

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

\textit{eTransJust} is identical to \textit{eTrans}, except when the value of
\textit{eTrans} is \textit{Nothing}.

\begin{code}
  let eTransJust = filterJust eTrans
\end{code}

Each \textit{GlobalTransmission} in \textit{eTransJust} is projected into
sender and receiver \textit{Event}s (these are the \textit{Event}s are defined
in \textit{Parser.lhs}).  Then, we unparse the \textit{GlobalTransmission} with
the unparsing function \textit{un}. \textit{srEventDesc} contains these
\textit{Event}s and the transmission description.
\par
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
%if False
\begin{code}
      -- [sender, receiver] in that order
\end{code}
%endif

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
\end{code}
%if False
        -- TODO probably lookup returns Nothing
%endif

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
\par

\begin{code}
      srXYDesc =
        ((\step ->
            \((sX, rX), desc) ->
              ( sX
              , rX
              , fromIntegral $ exYOffset + (-step * transmitStepCountSpace)
\end{code}
%if False
              -- negative because time increases downwards
%endif
\begin{code}
              , desc)) <$>
         bStepCount) <@>
        srXDesc
  return srXYDesc
\end{code}

%if False
\begin{code}
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

\textit{transmit} defines a transmission as a picture.  The transmission is
drawn as a pictorial arrow.  This pictorial arrow does not have any origin
coordinate.  \textit{transmit} takes four \textit{Float}s as input: the arrow
body length, the arrow head height, the arrow head length, and the arrow
thickness.
\par

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

%if False
\begin{code}
-- Draws transmit from sender to receiver with description
-- Translate transmits so that tail is at sender and head is at receiver
-- Description is always on the left of transmit
-- Flips transmits if needed
\end{code}
%endif

\textit{transmitSRDesc} defines a transmission with an origin coordinate as a
pictorial arrow.  The x-coordinate of the origin is always the sender of the
transmission's x-coordinate.  The direction of the pictorial arrow depends on
the x-coordinate of the receiver relative to the x-coordinate of the sender.
If the receiver's x-coordinate is greater than the sender's x-coordinate, then
the arrow faces right.  If the receiver's x-coordinate is less than the
sender's x-coordinate, then the arrow faces left.  Otherwise, we raise an
error, because the sender and the receiver have the same coordinate (up to
float precision).
\par

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

%if False
\begin{code}
-- All party extents in one line at the top
\end{code}
%endif

\textit{getPartiesExtents} creates an \textit{Extent} for each party.  An
\textit{Extent} is a rectangular area.  The width and height of each
\textit{Extent}, and the spacing between two \textit{Extent}s, are given as
\textit{w}, \textit{h}, and \textit{s} respectively.
\par

\begin{code}
getPartiesExtents :: [String] -> Int -> Int -> Int -> [Extent]
getPartiesExtents ss w h s =
  [ makeExtent (h + exYOffset) exYOffset (x + w + exXOffset) (x + exXOffset)
  | x <- take num [0,(w + s) ..]
  ]
  where
    num = length ss
\end{code}

%if False
\begin{code}
mappingPartyExtent :: [String] -> [(String, Extent)]
mappingPartyExtent ss = zip ss (getPartiesExtents ss exWidth exHeight exSpace)
\end{code}
%endif
