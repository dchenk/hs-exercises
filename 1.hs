-- [Tsoding] Tracking Hikes
-- Expected answer: 2
input = "UDDDUDUUDU"

solution :: String -> Int
solution =
  -- Accumulated value is (previous level, number of valleys)
  snd . foldl countValleys (0, 0)
  where
    nextLevel :: Int -> Char -> Int
    nextLevel level step = level + (if step == 'U' then 1 else -1)
    countValleys (prevLevel, numValleys) currStep =
      (nextLevel prevLevel currStep, numValleys + (if prevLevel == 0 && currStep == 'D' then 1 else 0))

main = interact $ show . solution
