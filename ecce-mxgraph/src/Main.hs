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
import Control.Monad (join)
import Data.JSString (JSString, pack)
import Data.List (intercalate, nub)
import Ecce.Parser
  ( AnyExpr(AnyExpr)
  , AnyExpr
  , Expr(..)
  , GlobalProtocol
  , parseExpr
  )
import Ecce.Processor
  ( Process
  , eventLoop
  , mayProcessToGlobalProtocol
  , networkProcessor
  , parseContents
  , projectGlobalToEndpoint
  )
import Ecce.Projector (projectPartyToEndpoint)
import Ecce.Unparser (un)
import Reactive.Banana (Behavior, Event, Moment, accumE, compile, liftMoment)
import Reactive.Banana.Frameworks
  ( MomentIO
  , actuate
  , changes
  , fromAddHandler
  , newAddHandler
  , reactimate
  , reactimate'
  )

foreign import javascript unsafe "new Show().show($1)" mx_show
  :: JSString -> IO ()

uml :: AnyExpr -> [String]
uml (AnyExpr e)
  -- Constructors defined in Expr
 =
  case e of
    EGlobalProtocolTransmission s i r c v f ->
      return $
      join $
      un (AnyExpr s) :
      "->" :
      un (AnyExpr r) :
      ":(" :
      un (AnyExpr i) :
      ")" :
      un (AnyExpr c) : "<" : un (AnyExpr v) : "." : un (AnyExpr f) : ">" : []
    EGlobalProtocolConcurrency g1 g2 ->
      "par" : uml (AnyExpr g1) ++ uml (AnyExpr g2) ++ "end" : []
    EGlobalProtocolChoice g1 g2 ->
      "alt choice 1" :
      uml (AnyExpr g1) ++ "else choice 2" : uml (AnyExpr g2) ++ "end" : []
    EGlobalProtocolSequencing g1 g2 ->
      join $ map uml (AnyExpr g1 : AnyExpr g2 : [])
    EGlobalProtocolAssumption a -> "Assumption(" : un (AnyExpr a) : ")" : []
    EGlobalProtocolGuard a -> "Guard(" : un (AnyExpr a) : ")" : []
    EGlobalProtocolEmp -> []

exportUml :: AnyExpr -> [String]
exportUml a = header : uml a ++ footer : []
  where
    header =
      "<mxGraphModel><root><mxCell id=\"0\"/><mxCell id=\"1\" parent=\"0\"/>"
    footer = "</mxCell></root></mxGraphModel>"

networkPrinter ::
     ( Event (Maybe (Expr GlobalProtocol))
     , Behavior (Maybe Process)
     , Event ()
     , Behavior Int)
  -> MomentIO ()
networkPrinter (eTrans, bProc, eDone, bStepCount) = do
  eTransAcc <-
    accumE EGlobalProtocolEmp (maybe id EGlobalProtocolSequencing <$> eTrans)
  reactimate $ (mx_show . pack . un . AnyExpr) <$> eTransAcc
  eProc <- changes bProc
  -- TODO find more efficient way of getting endpoint protocols
  reactimate' $
    fmap
      (putStrLn .
       intercalate "\n" .
       nub .
       map (un . AnyExpr) . projectGlobalToEndpoint . mayProcessToGlobalProtocol) <$>
    eProc
  eStepCount <- changes bStepCount
  reactimate' $ fmap (mx_show . pack . show) <$> eStepCount
  reactimate $ (mx_show . pack) "Done!" <$ eDone

networkDescription :: Event Char -> MomentIO ()
networkDescription eKey =
  (liftMoment $
   networkProcessor
     (fmap
        head
        (parseContents $
         Right
           [ "b1--(1)->s:s<1.E1.emp^1:order>; (s--(2)->b1:b1<2.E2.emp^2:price>*s--(3)->b2:b2<3.E3.emp^3:price>); b1--(4)->b2:b2<4.E4.emp^4:amt>; (b2--(5)->s:s<5.E5.emp^5:no>| (b2--(6)->s:s<6.E6.emp^6:yes>; b2--(7)->s:s<7.E7.emp^7:addr>))"
           ]))
     (Just <$> eKey)) >>=
  networkPrinter

main :: IO ()
main = do
  (addKeyEvent, fireKey) <- newAddHandler
  network <- compile $ fromAddHandler addKeyEvent >>= networkDescription
  actuate network
  eventLoop fireKey network
