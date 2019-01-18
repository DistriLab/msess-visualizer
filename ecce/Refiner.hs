{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE GADTs #-} -- Allows constrained ASTs
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns
{-# LANGUAGE EmptyDataDecls #-} -- Allows datatypes without constructors

{-
 - SECTION MODULE
 -}
module Refiner where

import Base (SParsec, extractParse)
import Control.Exception (SomeException)
import qualified Control.Exception (try)
import Data.Map (Map, alter, fromList, keys, lookup, singleton, union)
import Interpreter (Output, mainHaskeline)
import Parser
  ( Channel
  , Expr(EAssertionConstraint, EAssertionEvent, EChannel,
     EConstraintHappens, EEvent, EGlobalProtocolAssumption,
     EGlobalProtocolChoice, EGlobalProtocolConcurrency,
     EGlobalProtocolEmp, EGlobalProtocolGuard,
     EGlobalProtocolSequencing, EGlobalProtocolTransmission, ELabel,
     ERole)
  , GlobalProtocol
  , Label
  , Role
  , parseExpr
  )
import qualified Parser (Event)

{-
 - SECTION IMPORTS
 -}
import Prelude hiding (lookup)
import System.IO (FilePath, readFile)

{-
 - SECTION USER INTERFACE
 -}
type Test = (Integer, String, String)

main :: IO ()
main = mainHaskeline commandOutputs incommandOutput

commandOutputs :: [(String, Output)]
commandOutputs =
  [ ( "help"
    , \(_, commands, _) ->
        mapM_ putStrLn $ "Here are a list of commands:" : commands)
  , ( "load"
    , \(_, _, restInputLine) -> parseFileLoad restInputLine >>= mapM_ putStrLn)
  , ( "test"
    , \(_, _, restInputLine) -> parseFileTest restInputLine >>= mapM_ putStrLn)
  ]

incommandOutput :: Output
incommandOutput =
  \(inputLine, _, _) -> putStrLn $ extractParseShow parseExpr inputLine

-- Parse file at filePath with shower
parseFile ::
     Show a
  => FilePath
  -> String
  -> (a -> String)
  -> ([String] -> [a])
  -> IO [String]
parseFile filePath s shower f = do
  xs <- extractFile filePath
  return $ either (\e -> s : "Error: " : e) (\xs -> map shower (f xs)) xs

parseFileLoad :: FilePath -> IO [String]
parseFileLoad filePath =
  parseFile
    filePath
    "Usage:\n\tload <relativepath>"
    (extractParseShow parseExpr)
    id

-- All test files must follow a strict format:
-- Must be in test/parser/ directory
-- File contents: must be even number of lines long
-- Each pair of lines is:
-- (1) Input expression
-- (2) Expected result of running (1)
parseFileTest :: FilePath -> IO [String]
parseFileTest filePath =
  parseFile
    ("test/parser/" ++ filePath)
    "Usage:\n\ttest filename\n\ttest list"
    parseTest
    (indexAndPair . splitEvenOdd)
  where
    splitEvenOdd :: [a] -> ([a], [a])
    splitEvenOdd = foldr (\x ~(xs2, xs1) -> (x : xs1, xs2)) ([], [])
    indexAndPair :: ([a], [a]) -> [(Integer, a, a)]
    indexAndPair = uncurry (zip3 [0 ..])

extractFile :: FilePath -> IO (Either [String] [String])
extractFile filePath = do
  xs <- (Control.Exception.try . fmap lines . readFile) filePath
  return $
    either (\(e :: SomeException) -> Left $ show e : []) (\xs -> Right xs) xs

parseTest :: Test -> String
parseTest (n, i, o) =
  if e == o
    then concat $ "#" : show n : ":\tP" : []
    else concat $
         "#" : show n : ":\tF\n\texpect\t" : o : "\n\tactual\t" : e : []
  where
    e = extractParseShow parseExpr i

{-
 - SECTION PARSERS
 -}
extractParseShow :: Show a => SParsec a -> String -> String
extractParseShow p s = either show show $ extractParse p s

{-
 - SECTION TYPES
 -}
{- Figure 4.4 -}
data Boundary
  = BoundaryEvent Event
  | BoundaryTransmission Transmission

data Event =
  Event Role
        Label

data Transmission =
  Transmission Role
               Label
               Role
               Channel

data BForm a where
  BFormSingleton :: a -> BForm a
  BFormConcurrency :: BForm a -> BForm a -> BForm a

data EForm a where
  EFormUndefined :: EForm a
  EFormBSingleton :: BForm a -> EForm a
  EFormChoice :: EForm a -> EForm a -> EForm a

type BEvent = EForm Event

type BTransmission = EForm Transmission

type RMap = Map Role BEvent

type CMap = Map Channel BTransmission

type Border = (RMap, CMap)

{- Protocol Summary -}
data Summary = Summary
  { backtier :: (RMap, CMap)
  , frontier :: (RMap, CMap)
  , assumptionSet :: [Expr GlobalProtocol]
  , guardSet :: [Expr GlobalProtocol]
  }

{-
 - SECTION COLLECT
 -}
collect :: Expr GlobalProtocol -> Summary
collect g =
  case g of
    EGlobalProtocolSequencing g1 g2 -> h1
      where h1 =
              Summary
                (fuse ";" (backtier s1) (backtier s2))
                (fuse ";" (frontier s2) (frontier s1))
                (assumptionSet s1 ++ assumptionSet s2 ++ a3Assumptions)
                (guardSet s1 ++ guardSet s2 ++ a3Guards)
            (a3Assumptions, a3Guards) =
              mergeAdjacent (frontier s1) (backtier s2)
            s1 = collect g1
            s2 = collect g2
    EGlobalProtocolConcurrency g1 g2 -> h2 (collect g1) (collect g2)
      where h2 s1 s2 =
              Summary
                (fuse "*" (backtier s1) (backtier s2))
                (fuse "*" (frontier s1) (frontier s2))
                (assumptionSet s1 ++ assumptionSet s2)
                (guardSet s1 ++ guardSet s2)
    EGlobalProtocolChoice g1 g2 -> h3 (collect g1) (collect g2)
      where h3 s1 s2 =
              Summary
                (fuse "|" (backtier s1) (backtier s2))
                (fuse "|" (frontier s1) (frontier s2))
                (assumptionSet s1 ++ assumptionSet s2)
                (guardSet s1 ++ guardSet s2)
    EGlobalProtocolTransmission s i r c _ _ ->
      Summary
        (k, gamma)
        (k, gamma)
        [ EGlobalProtocolAssumption (EAssertionEvent (EEvent s i))
        , EGlobalProtocolAssumption (EAssertionEvent (EEvent r i))
        , EGlobalProtocolAssumption
            (EAssertionConstraint (EConstraintHappens (EEvent s i) (EEvent r i)))
        ]
        []
      where ERole s' = s
            ERole r' = r
            EChannel c' = c
            ELabel i' = i
            k :: RMap
            k =
              fromList
                [ (s', EFormBSingleton (BFormSingleton (Event s' i')))
                , (r', EFormBSingleton (BFormSingleton (Event r' i')))
                ]
            gamma :: CMap
            gamma =
              singleton
                c'
                (EFormBSingleton (BFormSingleton (Transmission s' i' r' c')))

-- Input: string as operator.
-- Split `op` by case.
-- Fuses two borders into one.
fuse :: String -> Border -> Border -> Border
fuse op (k1, gamma1) (k2, gamma2) = (k, gamma)
  where
    k =
      foldl
        (\m p -> alter (const $ fuseAux <$> lookup p k1 <*> lookup p k2) p m)
        k1
        (keys k2)
    gamma =
      foldl
        (\m c ->
           alter (const $ fuseAux <$> lookup c gamma1 <*> lookup c gamma2) c m)
        gamma1
        (keys gamma2)
    fuseAux :: EForm a -> EForm a -> EForm a
    fuseAux b1 b2 =
      case op of
        ";" ->
          case b1 of
            EFormUndefined -> b2
            otherwise -> b1
        "*" ->
          case b2 of
            EFormUndefined -> b1
            otherwise ->
              case b1 of
                EFormUndefined -> b2
                otherwise -> EFormBSingleton (BFormConcurrency b1' b2')
                  where EFormBSingleton b1' = b1
                        EFormBSingleton b2' = b2
        "|" -> EFormChoice b1 b2

mergeAdjacent ::
     Border -> Border -> ([Expr GlobalProtocol], [Expr GlobalProtocol])
mergeAdjacent (k1, gamma1) (k2, gamma2) = (k1 ^^ k2, gamma1 ^^ gamma2)
  where
    (^^) :: Ord k => Map k a -> Map k a -> [Expr GlobalProtocol]
    m1 ^^ m2 =
      concatMap
        (\k ->
           merge
             (maybe
                (error "Key does not exist in map")
                (EFormBSingleton . BFormSingleton)
                (lookup k m1))
             (maybe
                (error "Key does not exist in map")
                (EFormBSingleton . BFormSingleton)
                (lookup k m2)))
        (keys m1 ++ keys m2)
    merge :: EForm Boundary -> EForm Boundary -> [Expr GlobalProtocol]
    merge f1 f2 =
      case f1 of
        EFormBSingleton (BFormConcurrency b1 b2) ->
          merge (EFormBSingleton b1) f2 ++ merge (EFormBSingleton b2) f2
        EFormChoice b1 b2 -> merge b1 f2 ++ merge b2 f2
        EFormBSingleton (BFormSingleton (BoundaryEvent (Event p1 i1))) ->
          case f2 of
            EFormBSingleton (BFormSingleton (BoundaryEvent (Event p2 i2))) ->
              [ EGlobalProtocolAssumption
                  (EAssertionConstraint
                     (EConstraintHappens
                        (EEvent (ERole p1) (ELabel i1))
                        (EEvent (ERole p2) (ELabel i2))))
              ]
            otherwise -> error "Pattern not found"
        EFormBSingleton (BFormSingleton (BoundaryTransmission (Transmission s1 i1 r1 _))) ->
          case f2 of
            EFormBSingleton (BFormSingleton (BoundaryTransmission (Transmission s2 i2 r2 _))) ->
              [ EGlobalProtocolGuard
                  (EAssertionConstraint
                     (EConstraintHappens
                        (EEvent (ERole s1) (ELabel i1))
                        (EEvent (ERole s2) (ELabel i2))))
              , EGlobalProtocolGuard
                  (EAssertionConstraint
                     (EConstraintHappens
                        (EEvent (ERole r1) (ELabel i1))
                        (EEvent (ERole r2) (ELabel i2))))
              ]
            otherwise -> error "Pattern not found"
        otherwise ->
          case f2 of
            EFormBSingleton (BFormConcurrency b1 b2) ->
              merge f1 (EFormBSingleton b1) ++ merge f1 (EFormBSingleton b2)
            EFormChoice b1 b2 -> merge f1 b1 ++ merge f1 b2
