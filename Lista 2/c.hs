-- Robô 1

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
    deriving (Eq, Show, Read)

result :: Int -> [Command] -> (Int, Int) -> (Int, Int)
result _ [] x = x
result d ((TurnLeft):xs) (a, b) = result (mod (d-1) 4) xs (a, b)
result d ((TurnRight):xs) (a, b) = result (mod (d+1) 4) xs (a, b)
result d ((Forward x):xs) (a, b) 
    | d == 0 = result d xs (a, b+x)
    | d == 1 = result d xs (a+x, b)
    | d == 2 = result d xs (a, b-x)
    | d == 3 = result d xs (a-x, b)
result d ((Backward x):xs) (a, b) 
    | d == 0 = result d xs (a, b-x)
    | d == 1 = result d xs (a-x, b)
    | d == 2 = result d xs (a, b+x)
    | d == 3 = result d xs (a+x, b)

destination :: (Int,Int) -> [Command] -> (Int,Int) 
destination x y = result 0 y x

main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result