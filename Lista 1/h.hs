-- Máquina de Somar

maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []
maquinaSomar (0:[]) = []
maquinaSomar (0:0:xs) = []
maquinaSomar (x:[]) = [x]
maquinaSomar (x:0:0:xs) = [x]
maquinaSomar (x:0:xs) = x:maquinaSomar(xs)
maquinaSomar (x:y:xs)
  | x+y == 0 = 0:maquinaSomar(xs)
  | otherwise = maquinaSomar((x+y):xs)

main = do
  lista <- getLine
  print $ maquinaSomar (read lista :: [Int])