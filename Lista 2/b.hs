-- O par Fibonacci e Palmeirinha

getFibo :: Int -> [Int]
getFibo 1 = [0]
getFibo 2 = [0, 1]
getFibo n = (getFibo (n-1)) ++ [last (getFibo (n-1)) + last (getFibo (n-2))]

evenList :: [Int] -> [Int]
evenList x = [a | a <- x, mod a 2 == 0] 

getPassword :: String -> String
getPassword s = s ++ (show(foldr (+) 0 (evenList (getFibo (length s)))))

main = do
    input <- getLine
    let result = getPassword input
    print result