-- Unzip com foldr

solve :: (u, u) -> ([u], [u]) -> ([u],[u])
solve (a, b) (as, bs) = ((a:as), (b:bs)) 

unzip' :: [(u,u)] -> ([u],[u])
unzip' xs = foldr solve ([], []) xs

main = interact $ show . unzip' . (read :: String -> [(Int,Int)])