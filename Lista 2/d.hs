-- Robô 2

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

switch :: Direction -> Int
switch North = 0
switch West = 3
switch South = 2 
switch East = 1

unswitch :: Int -> Direction
unswitch 0 = North
unswitch 3 = West
unswitch 2 = South
unswitch 1 = East

faces :: Direction -> [Command] -> Direction
faces d [] = d
faces d (x:xs) 
    | x == TurnLeft = faces (unswitch (mod ((switch d) - 1) 4)) xs
    | x == TurnRight = faces (unswitch (mod ((switch d) + 1) 4)) xs
    | otherwise = faces d xs
    
main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result