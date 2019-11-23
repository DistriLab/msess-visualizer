module Main where

import Data.JSString (JSString, pack)
import Ecce.Parser (extractFile)
import Ecce.Processor (networkDescription)
import Ecce.Unparser (un)
import Reactive.Banana (Behavior, Event, Moment, liftMoment)

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
  eTransAcc <- accumE [] ((:) <$> eTrans)
  reactimate $
    maybe (return ()) (mx_show . pack . putStrLn . un . AnyExpr) <$> eTransAcc
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
  reactimate' $ fmap (putStrLn . show) <$> eStepCount
  reactimate $ putStrLn "Done!" <$ eDone

main :: IO ()
main = putStrLn "Hello, Haskell!"
