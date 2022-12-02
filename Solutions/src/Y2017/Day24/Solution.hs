-- Description : Advent of Code - 2017 / Tag 24
--
-- Siehe [Problestellung](https://adventofcode.com/2017/day/24)
module Y2017.Day24.Solution where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntMap as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.IntSet as Set
import qualified Text.Megaparsec.Char as PC
import Tools.CommonParsers (Parser)
import qualified Tools.CommonParsers as CP

yearNr :: Int
yearNr = 2017

dayNr :: Int
dayNr = 24

-- | Lösung des Tages berechnen und ausgeben
run :: IO ()
run = do
  putStrLn $ "YEAR " <> show yearNr <> "/ DAY " <> show dayNr

  input <- loadInput

  let result1 = part1 input
  putStrLn $ "\t Part 1: " ++ show result1

  let result2 = part2 input
  putStrLn $ "\t Part 2: " ++ show result2

  putStrLn "---\n"

----------------------------------------------------------------------
-- solutions

part1 :: Input -> Int
part1 =
  maximum . map strength . bridges . createConnections

part2 :: Input -> Int
part2 inp = maximum $ map strength allLongest
 where
  allBridges = bridges $ createConnections inp
  lengthLongest = maximum $ map length allBridges
  allLongest = filter (\b -> length b == lengthLongest) allBridges

----------------------------------------------------------------------
-- data model

type Input = [Componente]

type Componente = (Port, Port)

type Port = Int

type Connections = IntMap IntSet

createConnections :: Input -> Connections
createConnections inp =
  Map.fromListWith Set.union $
    concat [[(a, Set.singleton b), (b, Set.singleton a)] | (a, b) <- inp]

type Bridge = [Componente]

bridges :: Connections -> [Bridge]
bridges connections = tail $ go connections 0
 where
  go :: Connections -> Port -> [Bridge]
  go con end =
    case IntSet.toList (con IntMap.! end) of
      [] -> [[]]
      fits ->
        [] : do
          next <- fits
          let comp = (end, next)
          let con' = removeComp comp con
          restBridge <- go con' next
          pure $ comp : restBridge

removeComp :: Componente -> Connections -> Connections
removeComp (a, b) cons =
  IntMap.update (Just . IntSet.delete b) a $
    IntMap.update (Just . IntSet.delete a) b cons

strength :: Bridge -> Int
strength comps =
  sum [a + b | (a, b) <- comps]

----------------------------------------------------------------------
-- load and parse input

loadInput :: IO Input
loadInput = loadFile "input.txt"

loadExample :: IO Input
loadExample = loadFile "example.txt"

loadFile :: FilePath -> IO Input
loadFile file = do
  txt <- readFile ("./src/Y" <> show yearNr <> "/Day" <> show dayNr <> "/" <> file)
  pure $ parseText txt

parseText :: String -> Input
parseText = map (CP.runParser componenteP) . lines

componenteP :: Parser Componente
componenteP = do
  portA <- CP.numberP
  _ <- PC.char '/'
  portB <- CP.numberP
  pure (portA, portB)
