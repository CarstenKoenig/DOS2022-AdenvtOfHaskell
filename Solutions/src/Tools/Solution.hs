-- Description : Advent of Code - 2015 / Tag 1
--
-- Siehe [Problestellung](https://adventofcode.com/2015/day/1)
module Tools.Solution where

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
part1 _ = 0

part2 :: Input -> Int
part2 _ = 0

----------------------------------------------------------------------
-- data model

type Input = ()

----------------------------------------------------------------------
-- load and parse input

loadInput :: IO Input
loadInput = loadFile "input.txt"

loadFile :: FilePath -> IO Input
loadFile file = do
  txt <- readFile ("./src/Y" <> show yearNr <> "/Day" <> show dayNr <> "/" <> file)
  pure $ parseText txt

parseText :: String -> Input
parseText txt =
  let _ = lines txt
   in ()