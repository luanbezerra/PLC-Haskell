-- Fatores Primos

solve :: Int -> Int -> Int -> Int -> [(Int, Int)]
solve n i e fix
  | (i > (floor (sqrt (fromIntegral fix)))) && (n > 1) = [(n, 1)]
  | (i > (floor (sqrt (fromIntegral fix)))) && (n <= 1) = []
  | mod n i == 0 = solve (div n i) i (e+1) fix
  | e > 0 = (i,e):(solve n (i+1) 0 fix)
  | otherwise = (solve n (i+1) 0 fix)

fatPrime :: Int -> [(Int, Int)]
fatPrime n = solve n 2 0 n

main = do
      a <- getLine
      let result = fatPrime (read a :: Int)
      print result