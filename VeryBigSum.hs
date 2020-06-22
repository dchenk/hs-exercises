-- Though this problem is easy using the Integer type, the point is to not use Integers.
import Data.Char

bigSum :: [String] -> String
bigSum = foldr addInts "0"

addInts :: String -> String -> String
addInts a b =
  snd $ foldr s (0, "") $ zip normA normB
  where
    (normA, normB) = normalizedInts a b
    s :: (Char, Char) -> (Int, String) -> (Int, String)
    s (digitA, digitB) (carry, acc) =
      let ab = digitToInt digitA + digitToInt digitB + carry
          prepend1 = if ab > 9 && length acc == length normA - 1 then "1" else ""
       in if ab > 9
            then (1, prepend1 ++ [last (show (10 - ab))] ++ acc)
            else (0, show ab ++ acc)

normalizedInts :: String -> String -> (String, String)
normalizedInts a b
  | lenA == lenB = (a, b)
  | lenA > lenB = (a, replicate (lenA - lenB) '0' ++ b)
  | otherwise = (replicate (lenB - lenA) '0' ++ a, b)
  where
    lenA = length a
    lenB = length b

main = do
  _ <- getLine
  line <- getLine
  putStrLn $ bigSum $ words line
