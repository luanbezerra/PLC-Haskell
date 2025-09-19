-- Uncommon Words

import Data.Char
import Data.List

getList :: String -> [String]
getList [] = []
getList x = case break (== ' ') x of
  (before, (' ':after)) -> ((map toLower before):(getList after))

unite :: [String] -> [String] -> [String]
unite [] [] = []
unite x [] = x
unite [] y = y
unite (x:xs) (y:ys) 
        | x <= y = x:(unite xs (y:ys))
        | otherwise = y:(unite (x:xs) ys)

solve :: [String] -> [String]
solve (x:[]) = [x]
solve (x:y:[]) = if (x /= y) then ([x, y]) else []
solve (x:y:xs) 
    | x == y && (x /= head xs) = solve xs
    | x == y = solve (y:xs)
    | otherwise = x:(solve (y:xs))

uncommonFromTwoSentences :: String -> String -> [String]
uncommonFromTwoSentences x y = solve (unite (sort (getList (x++" "))) (sort(getList (y++" "))))

main = do
sentence_1 <- getLine
sentence_2 <- getLine
let result = uncommonFromTwoSentences sentence_1 sentence_2
print result