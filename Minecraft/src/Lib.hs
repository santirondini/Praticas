

type Cantidad = Int
type Material = String

data Receta = UnaReceta {
    materiales :: [Material],
    tiempo :: Int
} deriving (Eq,Show)

data Personaje = UnPersonaje {
    nombre :: String,
    puntaje :: Int,
    inventario:: [Material]
} deriving (Eq,Show)

fogata = UnaReceta ["madera","fosforo"] 10
polloasado= UnaReceta ["pollo","fogata"] 300
sueter = UnaReceta ["lana","agujas","tintura"] 600

steve = UnPersonaje "Steve" 1500 ["lana","agujas","tintura","madera","fosforo","pollo"]

borrar :: String->[String]->[String]
borrar  _ [] = []
borrar aborrar (x:xs)
                    | x == aborrar = borrar aborrar xs
                    | otherwise = x : borrar aborrar xs

tieneLosElementosDeLaReceta :: Personaje->Receta->Bool
tieneLosElementosDeLaReceta personaje receta = all (\ingrediente -> ingrediente `elem` inventario personaje) (materiales receta)

meterFogata :: Personaje->Personaje
meterFogata personaje = personaje {inventario  = "fogata" : borrar "fosforo" (borrar "madera" (inventario personaje))}

meterSueter :: Personaje->Personaje
meterSueter personaje = personaje {inventario = "sueter" : borrar "lana" (borrar "agujas" (borrar "tintura" (inventario personaje)))}

meterPolloAsado :: Personaje->Personaje
meterPolloAsado personaje = personaje {inventario = "pollo asado" : borrar "fogata" (borrar "pollo" (inventario personaje)) }

incrementoPuntos :: Receta->Personaje->Personaje
incrementoPuntos receta personaje = personaje {puntaje = puntaje personaje + tiempo receta*10}

craftearFogata :: Receta->Personaje->Personaje
craftearFogata fogata personaje
                        | tieneLosElementosDeLaReceta personaje fogata =  incrementoPuntos fogata (meterFogata personaje)
                        | otherwise = personaje

craftearPolloAsado :: Receta->Personaje->Personaje 
craftearPolloAsado polloasado personaje
                        | tieneLosElementosDeLaReceta personaje polloasado = incrementoPuntos polloasado (meterPolloAsado personaje)
                        | otherwise = personaje

craftearSueter :: Receta->Personaje->Personaje
craftearSueter sueter personaje 
                        | tieneLosElementosDeLaReceta personaje sueter = incrementoPuntos sueter (meterSueter personaje)
                        | otherwise = personaje

craftear :: Receta->Personaje->Personaje
craftear fogata personaje = craftearFogata fogata personaje
craftear sueter personaje = craftearSueter sueter personaje
craftear polloasado personaje = craftearPolloAsado polloasado personaje

--hacerPolloAsado :: Personaje->Personaje
--hacerPolloAsado personaje 