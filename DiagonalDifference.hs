solution :: [Int] -> Int
solution (n : ns) =
  abs $ foldl diff 0 [0 .. (n - 1)]
  where
    oppositeI i = n - 1 - i
    diff currDiff i = currDiff + (ns !! (i * n + i)) - (ns !! (i * n + oppositeI i))

main = interact $ show . solution . map read . words
