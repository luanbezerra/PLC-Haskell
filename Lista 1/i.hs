-- Lista de múltiplos da lista

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos [] m = []
somarMultiplos (x:xs) 0 = 0:(somarMultiplos xs 0)
somarMultiplos (h:t) m = (sumList [x | x <- [m,m+m..h]]):(somarMultiplos t m)

main = do
    lista <- getLine
    let readList = read lista :: [Int]
    num <- getLine
    let readNum = read num :: Int
    let result = somarMultiplos readList readNum
    print result