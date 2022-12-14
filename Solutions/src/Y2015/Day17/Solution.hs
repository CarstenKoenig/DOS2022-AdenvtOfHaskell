-- Description : Advent of Code - 2015 / Tag 17
--
-- Siehe [Problestellung](https://adventofcode.com/2015/day/17)
module Y2015.Day17.Solution where

import Data.Function.Memoize (memoFix2)

yearNr :: Int
yearNr = 2015

dayNr :: Int
dayNr = 17

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
part1 = length . combsFor 150

part2 :: Input -> Int
part2 = minCombsFor 150

----------------------------------------------------------------------
-- data model

type Input = [Container]

type Container = Int
type Volume = Int

minCombsFor :: Volume -> [Container] -> Int
minCombsFor vol conts =
  length $ filter (\cs -> length cs == minCount) allCombs
 where
  allCombs = combsFor vol conts
  minCount = minimum $ map length allCombs

-- >>> length (combsFor 25 [20,15,10,5,5]) == 4
-- True
combsFor :: Volume -> [Container] -> [[Container]]
combsFor 0 _ = [[]]
combsFor _ [] = []
combsFor vol (cont : more)
  | cont > vol = combsFor vol more
  | otherwise =
      [cont : comb | comb <- combsFor (vol - cont) more]
        ++ combsFor vol more

combsFor' :: Volume -> [Container] -> [[Container]]
combsFor' = memoFix2 go
 where
  go _ 0 _ = [[]]
  go _ _ [] = []
  go f vol (cont : more)
    | cont > vol = f vol more
    | otherwise =
        [cont : comb | comb <- f (vol - cont) more]
          ++ f vol more

----------------------------------------------------------------------
-- load and parse input

loadInput :: IO Input
loadInput = loadFile "input.txt"

loadFile :: FilePath -> IO Input
loadFile file = do
  txt <- readFile ("./src/Y" <> show yearNr <> "/Day" <> show dayNr <> "/" <> file)
  pure $ parseText txt

parseText :: String -> Input
parseText = map read . lines
