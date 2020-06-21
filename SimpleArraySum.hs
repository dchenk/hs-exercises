{-# LANGUAGE TypeApplications #-}

-- Sample input:
--6
--1 2 3 4 10 11

main :: IO ()
main = do
  _ <- getLine
  line <- getLine
  print $ sum $ map (read @Int) $ words line
