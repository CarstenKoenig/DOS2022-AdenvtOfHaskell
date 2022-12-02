-- Description : Advent of Code - 2015 / Tag 1
--
-- Siehe [Problestellung](https://adventofcode.com/2015/day/1)
module Y2015.Day1.Solution where

import Data.Foldable (Foldable (foldl'))

yearNr :: Int
yearNr = 2015

dayNr :: Int
dayNr = 1

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
part1 = moves

part2 :: Input -> Int
part2 _ = 0

----------------------------------------------------------------------
-- data model

type Input = [Move]

data Move = Up | Down
  deriving (Show)

type Floor = Int

start :: Floor
start = 0

move :: Floor -> Move -> Floor
move f Up = f + 1
move f Down = f - 1

moves :: Input -> Floor
moves = foldl' move start

-- >>> firstPosBasement  [Down] == 1
-- False
-- >>> firstPosBasement [Up, Down, Up, Down, Down] == 5
-- False
firstPosBasement :: Input -> Int
firstPosBasement inp = 1

----------------------------------------------------------------------
-- load and parse input

loadInput :: IO Input
loadInput = loadFile "input.txt"

loadFile :: FilePath -> IO Input
loadFile file = do
  txt <- readFile ("./src/Y" <> show yearNr <> "/Day" <> show dayNr <> "/" <> file)
  pure $ parseText txt

parseText :: String -> Input
parseText = map parseMove
 where
  parseMove '(' = Up
  parseMove ')' = Down
  parseMove _ = error "invalid Input"
