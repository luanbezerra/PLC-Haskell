-- Binário para inteiro

btoi :: String -> Int
btoi [] = 0
btoi (x:xs)
  | x == '1' = 2^(length xs) + btoi xs
  | x == '0' = btoi xs

main = do
    s <- getLine
    let result = btoi s
    print result