-- Árvore de busca binária 1

data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

preOrder :: Ord t => Tree t -> [t]
preOrder (Nilt) = []
preOrder (Node v l r) = (preOrder l) ++ [v] ++ (preOrder r)

checkk :: Ord t => [t] -> Bool
checkk [] = True
checkk (x:[]) = True
checkk (x:y:xs) = (&&) (x < y) (checkk (y:xs))

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node x r s) = checkk (preOrder (Node x r s))


main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result