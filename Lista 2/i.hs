-- DNA1

data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

switch :: Int -> Char
switch x
    | mod x 5 == 0 = 'E'
    | mod x 5 == 1 = 'M'
    | mod x 5 == 2 = 'A'
    | mod x 5 == 3 = 'C'
    | mod x 5 == 4 = 'S'

inOrder :: Tree Int -> String
inOrder (Nilt) = ""
inOrder (Node x l r) = (inOrder l) ++ [(switch x)] ++ (inOrder r)

solve :: String -> [String]
solve [] = []
solve s = (take 8 s):(solve $ drop 8 s)

dna1 :: Tree Int -> [String]
dna1 x = solve $ inOrder x

main :: IO ()
main = do
  input <- getLine
  let result = dna1 (read input :: Tree Int)
  print result