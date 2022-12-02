-- Description : Advent of Code - 2016 / Tag 13
--
-- Siehe [Problestellung](https://adventofcode.com/2016/day/13)
module Y2016.Day13.Solution where

import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showBin)
import Tools.Coords (Coord, fourNeighbours)

yearNr :: Int
yearNr = 2016

dayNr :: Int
dayNr = 13

-- | Lösung des Tages berechnen und ausgeben
run :: IO ()
run = do
  putStrLn $ "YEAR " <> show yearNr <> "/ DAY " <> show dayNr

  let result1 = part1 input
  putStrLn $ "\t Part 1: " ++ show result1

  let result2 = part2 input
  putStrLn $ "\t Part 2: " ++ show result2

  putStrLn "---\n"

----------------------------------------------------------------------
-- solutions

part1 :: Input -> Int
part1 inp = nrStepsTo inp (31, 39)

part2 :: Input -> Int
part2 inp = Set.size $ placesInSteps inp 50

-- >>> isOpen 10 (0,0)
-- True
-- >>> isOpen 10 (2,1)
-- False
isOpen :: Input -> Coord -> Bool
isOpen inp (x, y) =
  even $ length $ filter (== '1') bin
 where
  value = x * x + 3 * x + 2 * x * y + y + y * y + inp
  bin = showBin value ""

-- >>> reachableFrom 10 (0,1)
-- [(0,0),(1,1)]
reachableFrom :: Input -> Coord -> [Coord]
reachableFrom inp coord =
  [ c
  | c@(x, y) <- fourNeighbours coord
  , x >= 0
  , y >= 0
  , isOpen inp c
  ]

start :: Coord
start = (1, 1)

type Step = Int

-- >>> nrStepsTo 10 (7,4)
-- 11
nrStepsTo :: Input -> Coord -> Step
nrStepsTo inp goal = go (Set.singleton start) [(0, start)]
 where
  go :: Set Coord -> [(Step, Coord)] -> Step
  go _ [] = error "did not find a way"
  go visited ((step, coord) : rest)
    | coord == goal = step
    | otherwise =
        go
          (Set.insert coord visited)
          ( rest
              ++ [ (step + 1, coord')
                 | coord' <- reachableFrom inp coord
                 , not (coord' `Set.member` visited)
                 ]
          )

placesInSteps :: Input -> Step -> Set Coord
placesInSteps inp maxSteps =
  go (Set.singleton start) [(0, start)]
 where
  go :: Set Coord -> [(Step, Coord)] -> Set Coord
  go vistied [] = vistied
  go visited ((step, coord) : rest)
    | step > maxSteps = go visited rest
    | otherwise =
        go
          (Set.insert coord visited)
          ( rest
              ++ [ (step + 1, coord')
                 | coord' <- reachableFrom inp coord
                 , not (coord' `Set.member` visited)
                 ]
          )

----------------------------------------------------------------------
-- data model

type Input = Int

input :: Input
input = 1362
