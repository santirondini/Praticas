
-- Modelo inicial
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


type Palo = Habilidad -> Tiro

constructorDePalos :: Int -> Int -> Int -> Tiro
constructorDePalos  veloc preci altu = UnTiro {velocidad = veloc, precision = preci, altura = altu}

patter :: Palo
patter habilidad = constructorDePalos 10 (2*precisionJugador habilidad) 0

madera :: Palo
madera habilidad = constructorDePalos 100 (div (precisionJugador habilidad) 2) 5

minimoCero n x
              | n-x < 0 = 0
              | otherwise = n-x

hierros :: Int -> Palo
hierros n habilidad = constructorDePalos (fuerzaJugador habilidad*n) (div (precisionJugador habilidad) n) (minimoCero n 3)

loshierros :: Int -> [Palo]
loshierros 0 = []
loshierros n = hierros n : loshierros (n-1)

palos = [patter,madera] ++ loshierros 10

--

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

-- 

type Obstaculo = Tiro -> Tiro
type PostObstaculo = Tiro -> Tiro

-- Condiciones:

mayorA:: (Tiro -> Int) -> Int -> Tiro -> Bool
mayorA caracteristica cantidad tiro = caracteristica tiro > cantidad

vaAlras :: Tiro -> Bool
vaAlras tiro = altura tiro < 1

entre :: (Tiro -> Int) -> Int -> Int -> Tiro -> Bool
entre caracteristica n x tiro = n < caracteristica tiro  && caracteristica tiro < x

cumple :: [Tiro -> Bool] -> Tiro -> Bool
cumple condiciones tiro = all (\condicion -> condicion tiro) condiciones

siSeCumple:: [Tiro -> Bool] -> Tiro -> PostObstaculo -> Tiro
siSeCumple condiciones tiro post
                                            | cumple condiciones tiro = post tiro
                                            | otherwise = tiro

condicionesParaTunel = [mayorA precision 90, vaAlras]
condicionesParaLaguna = [mayorA velocidad 80, entre altura 1 5]
condicionesParaHoyo = [entre velocidad 5 20, vaAlras, mayorA precision 95]


cambiarVelocidad :: Int -> PostObstaculo
cambiarVelocidad cantidad tiro = tiro {velocidad = cantidad}

cambiarPrecision :: Int -> PostObstaculo
cambiarPrecision cantidad tiro = tiro {precision = cantidad}

cambiarAltura :: Int -> PostObstaculo
cambiarAltura cantidad tiro = tiro {altura = cantidad}

postTunel :: Tiro -> Tiro
postTunel tiro = cambiarAltura 0 (cambiarPrecision 100 (cambiarVelocidad (velocidad tiro*2) tiro))   

postLaguna :: Int -> Tiro -> Tiro
postLaguna largo tiro = cambiarAltura (div (altura tiro) largo) tiro 

postHoyo :: PostObstaculo
postHoyo tiro = cambiarVelocidad 0 (cambiarPrecision 0(cambiarAltura 0 tiro))  

tunel :: Obstaculo
tunel tiro = siSeCumple condicionesParaTunel tiro postTunel

laguna :: Int -> Obstaculo
laguna largo tiro = siSeCumple condicionesParaLaguna tiro (postLaguna largo)

hoyo :: Obstaculo
hoyo tiro = siSeCumple condicionesParaHoyo tiro postHoyo











