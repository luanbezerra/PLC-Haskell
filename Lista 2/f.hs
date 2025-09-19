-- Plano 3D

data Mov = Z Double | X Double | Y Double
                  deriving (Eq, Show, Read)

type Point3D = (Double, Double, Double)

type Object3D = [Point3D]

fst3 :: Ord u => (u, u, u) -> u
fst3 (a, b, c) = a

snd3 :: Ord u => (u, u, u) -> u
snd3 (a, b, c) = b

trd3 :: Ord u => (u, u, u) -> u
trd3 (a, b, c) = c
 
toRad :: Double -> Double
toRad x = (x/180)*pi

sumAxis :: Object3D -> Mov -> Object3D
sumAxis [] _ = []
sumAxis (t:ts) (X x) = ((fst3 t) + x, (snd3 t), (trd3 t)):(sumAxis ts (X x))
sumAxis (t:ts) (Y x) = ((fst3 t), (snd3 t) + x, (trd3 t)):(sumAxis ts (Y x))
sumAxis (t:ts) (Z x) = ((fst3 t), (snd3 t), (trd3 t) + x):(sumAxis ts (Z x))

transladaObjeto :: Object3D -> [Mov] -> Object3D
transladaObjeto x [] = x
transladaObjeto t (x:xs) = transladaObjeto (sumAxis t x) xs

rotX :: Object3D -> Double -> Object3D
rotX [] _ = []
rotX (t:ts) x = (fst3 t, (cos x)*(snd3 t) - (sin x)*(trd3 t), (sin x)*(snd3 t) + (cos x)*(trd3 t)):(rotX ts x)

rotY :: Object3D -> Double -> Object3D
rotY [] _ = []
rotY (t:ts) x = ((cos x)*(fst3 t) + (sin x)*(trd3 t), snd3 t, (cos x)*(trd3 t) - (sin x)*(fst3 t)):(rotY ts x)

rotZ :: Object3D -> Double -> Object3D
rotZ [] _ = []
rotZ (t:ts) x = ((cos x)*(fst3 t) - (sin x)*(snd3 t), (sin x)*(fst3 t) + (cos x)*(snd3 t), trd3 t):(rotZ ts x)

rotacionaObjeto :: Double -> Double -> Double -> Object3D -> Maybe Object3D
rotacionaObjeto x y z c 
    | (x < 0 || x > 360 || y < 0 || y > 360 || z < 0 || z > 360) = Nothing
    | otherwise = Just (rotX (rotY (rotZ c (toRad z)) (toRad y)) (toRad x))
       
main = do
       coord <- getLine
       mov <- getLine
       angX <- getLine
       angY <- getLine
       angZ <- getLine
       let transladado = transladaObjeto (read coord :: Object3D) (read mov :: [Mov])
       let rotacionado = rotacionaObjeto (read angX) (read angY) (read angZ) transladado
       print rotacionado