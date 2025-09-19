-- Estatísticas da fatura do cartão

fix :: String -> String
fix [] = []
fix (';':';':xs) = fix (';':xs)
fix (x:xs) = x:(fix xs)

solve :: String -> [Double]
solve [] = []
solve x = case break (== ';') x of
  (before, after) -> (read before):(solve(drop 1 after))

getEven :: [Double] -> [Double]
getEven [] = []
getEven (x:xs)
  | mod (length xs) 2 == 1 = getEven xs
  | otherwise = x:(getEven xs) 
 
minMaxCartao :: String -> (Double, Double)
minMaxCartao x = (minimum (getEven (solve (fix ([a | a <- x, a >= '.', a <= ';'])))), maximum (getEven(solve (fix ([a | a <- x, a >= '.', a <= ';'])))))

main = do
    a <- getLine
    let result = minMaxCartao a
    print result