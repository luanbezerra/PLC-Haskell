-- Soma seletiva

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

sumNumbers :: String -> Int
sumNumbers [] = 0
sumNumbers (x:xs)
  | (x >= '0' && x <= '9') = (charToInt x) + sumNumbers xs
  | otherwise = sumNumbers xs

main = do
  a <- getLine
  let result = sumNumbers a
  print result