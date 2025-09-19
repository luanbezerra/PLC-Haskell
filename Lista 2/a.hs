-- Maior diâmetro

data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

maiorDiametro :: Ord t => Tree t -> Int
maiorDiametro (Nilt) = 0
maiorDiametro (Node x l r) = max (1 + (getHeight l) + (getHeight r)) (max (maiorDiametro l) (maiorDiametro r)) 

getHeight :: Ord t => Tree t -> Int
getHeight (Nilt) = 0
getHeight (Node x l r) = 1 + max (getHeight l) (getHeight r)
              
main = do
       s <- getLine
       let result = maiorDiametro (read s::Tree Int)
       print result