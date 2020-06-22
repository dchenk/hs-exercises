solution :: [Int] -> [Int] -> [Int]
solution aScores bScores =
  foldl eachScores [0, 0] $ zip aScores bScores

eachScores :: [Int] -> (Int, Int) -> [Int]
eachScores [aScore, bScore] (a, b)
  | a > b = [aScore + 1, bScore]
  | b > a = [aScore, bScore + 1]
  | otherwise = [aScore, bScore]

lineInts :: String -> [Int]
lineInts = map read . words

main = do
  aScores <- getLine
  bScores <- getLine
  putStrLn $ unwords $ map show $ solution (lineInts aScores) (lineInts bScores)
