-- Decifre o enigma

switch :: [(Char, Char)] -> Char -> Char
switch [] c = c
switch (x:xs) c 
  | fst x == c = snd x
  | otherwise = switch xs c

decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] t = []
decEnigma (x:xs) t = (switch t x):decEnigma xs t

main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result