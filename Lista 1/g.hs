-- Fatura do cartão

fix :: String -> String
fix [] = []
fix (';':';':xs) = fix (';':xs)
fix (x:xs) = x:(fix xs)

tuplas :: [Double] -> [String] -> [(Double, String)]
tuplas [] x = []
tuplas x [] = []
tuplas (x:xs) (y:ys) = (x, y):(tuplas xs ys)

solve :: String -> [Double]
solve [] = []
solve x = case break (== ';') x of
  (before, after) -> (read before):(solve(drop 1 after))

solve2 :: String -> [String]
solve2 [] = []
solve2 x = case break (== ';') x of
  (before, after) -> if (head before >= '0' && head before <= '9' && ((last before <= 'Z' && last before >= 'A') || (last before <= 'z' && last before >= 'a'))) then ((drop 3 before):(solve2 (drop 1 after)))
                     else (solve2 (drop 1 after))

getEven :: [Double] -> [Double]
getEven [] = []
getEven (x:xs)
  | mod (length xs) 2 == 1 = getEven xs
  | otherwise = x:(getEven xs)

sumMonth :: String -> [(Double, String)] -> [Double]
sumMonth s [] = []
sumMonth s (x:xs) 
  | snd x == s = (fst x):(sumMonth s xs)
  | otherwise = sumMonth s xs


logMes :: String -> String -> Double
logMes month s = foldl (+) 0 (sumMonth (month) (tuplas (getEven (solve (fix ([a | a <- s, a >= '.', a <= ';'])))) (solve2 s))) 

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result