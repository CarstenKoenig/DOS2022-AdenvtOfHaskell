-- Description : Advent of Code - 2015 / Tag 1
--
-- Siehe [Problestellung](https://adventofcode.com/2015/day/1)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Y2017.Day12.Solution where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import Tools.CommonParsers (Parser)
import qualified Tools.CommonParsers as CP

yearNr :: Int
yearNr = 2017

dayNr :: Int
dayNr = 12

-- | Lösung des Tages berechnen und ausgeben
run :: IO ()
run = do
  putStrLn $ "YEAR " <> show yearNr <> "/ DAY " <> show dayNr

  input <- loadInput
  let graph = createGraph input

  let result1 = part1 graph
  putStrLn $ "\t Part 1: " ++ show result1

  let result2 = part2 graph
  putStrLn $ "\t Part 2: " ++ show result2

  putStrLn "---\n"

----------------------------------------------------------------------
-- solutions

part1 :: Graph -> Int
part1 g =
  Set.size $ connectedGroupOf g 0

part2 :: Graph -> Int
part2 = length . connectedGroups

----------------------------------------------------------------------
-- data model

type Input = [Definition]

type PrgId = Int

data Definition = Definition
  { prgId :: PrgId
  , canComWith :: IntSet
  }
  deriving (Show)

type Definitions = IntMap Definition

createDefinitions :: Input -> Definitions
createDefinitions defs =
  IntMap.fromList [(prgId d, d) | d <- defs]

type Graph = IntMap IntSet

createGraph :: Input -> Graph
createGraph defs =
  IntMap.fromList [(prgId d, canComWith d) | d <- defs]

connectedGroupOf :: Graph -> PrgId -> IntSet
connectedGroupOf graph pId =
  go Set.empty (Set.singleton pId)
 where
  go visited lookAt =
    case Set.minView lookAt of
      Nothing -> visited
      Just (prgId', rest) ->
        let visited' = Set.insert prgId' visited
            reachable = graph IntMap.! prgId'
            shouldBeVisted = Set.difference reachable visited'
         in go visited' (Set.union rest shouldBeVisted)

connectedGroups :: Graph -> [IntSet]
connectedGroups graph = go allNodes
 where
  go :: IntSet -> [IntSet]
  go nodes =
    case Set.minView nodes of
      Nothing -> []
      Just (point, rest) ->
        let connected = connectedGroupOf graph point
         in connected : go (Set.difference rest connected)
  -- reicht - weil selbst wenn eine Id nur in
  -- canComWith wäre würde sie hier irgendwo mit
  -- in eine Komponente aufgenommen
  -- ODER: ist ein Bidirektionaler Graph - sonst ist
  -- der Input falsch (danke an den Teilnehmer Christopher)
  allNodes = Set.fromList (IntMap.keys graph)

----------------------------------------------------------------------
-- load and parse input

loadInput :: IO Input
loadInput = loadFile "input.txt"

loadFile :: FilePath -> IO Input
loadFile file = do
  txt <- readFile ("./src/Y" <> show yearNr <> "/Day" <> show dayNr <> "/" <> file)
  pure $ parseText txt

parseText :: String -> Input
parseText = map (CP.runParser definitionP) . lines

-- >>> CP.runParser prgIdP "3"
-- 3
prgIdP :: Parser PrgId
prgIdP = CP.numberP

-- >>> CP.runParser canComWithP "0, 3, 4"
-- fromList [0,3,4]
canComWithP :: Parser IntSet
canComWithP = do
  prgs <- prgIdP `P.sepBy1` PC.string ", "
  return $ Set.fromList prgs

-- >>> CP.runParser definitionP "2 <-> 0, 3, 4"
-- Definition {prgId = 2, canComWith = fromList [0,3,4]}
definitionP :: Parser Definition
definitionP = do
  pId <- prgIdP
  _ <- PC.string " <-> "
  canCom <- canComWithP
  pure $ Definition pId canCom

-- >>> test
-- [(1,2),(1,3),(1,4),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,2),(4,3),(4,4),(5,2),(5,3),(5,4)]
test = do
  x <- [1 .. 5]
  y <- [2 .. 4]
  pure (x, y)
