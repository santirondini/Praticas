-- Mini Golfito:

--Funciones dadas por el parcial:

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

habilidad1 = Habilidad 20 20

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

--1) MODELAR PALOS: 

type Palo = Habilidad->Tiro

-- Defino un tirobase ya que no puedo agarrar una habildad y devolver un tiro. CREO que no puedo sacar un tipo de dato a partir de otro:
tirobase = UnTiro 0 0 0

patter :: Palo
patter habilidad = tirobase {velocidad = 10, precision = 2*precisionJugador habilidad, altura = 0}

madera :: Palo
madera habilidad = tirobase {velocidad = 100, precision = div (precisionJugador habilidad) 2 , altura = 5}

alturaAPartirDe3 :: Int->Int
alturaAPartirDe3 n
                   | (n-3) <= 0 = 0
                   | otherwise = n-3

hierro :: Int->Palo
hierro n habilidad = tirobase {velocidad = fuerzaJugador habilidad*n, precision = div (precisionJugador habilidad) n , altura = alturaAPartirDe3 n}

palos :: [Palo]
palos = [patter, madera , hierro 1, hierro 2, hierro 3, hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

{- 
2) Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.
-}

type TiroResultante = Tiro

golpe :: Jugador->Palo->TiroResultante
golpe jugador palo = palo (habilidad jugador)

{-
3) Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. 
En principio necesitamos representar los siguientes obstáculos:
          a) Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. 
          Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.
          b) Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros.
           Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.
          c) Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
          Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
-}

type Obstaculo = Tiro->Tiro
type Condicion = Tiro->Bool
type LargoDeLaguna = Int

tiroEnCero :: Tiro->Tiro
tiroEnCero tiro = tiro {velocidad = 0, precision= 0, altura= 0}

--Condiciones: 

siLaPrecisionEsMayorA90 :: Condicion
siLaPrecisionEsMayorA90 tiro = precision tiro > 90

condicionParaPasarHoyo :: Condicion
condicionParaPasarHoyo tiro = (velocidad tiro >= 5 && velocidad tiro <= 20) && (precision tiro > 95)

condicionParaPasarLaLaguna :: Condicion
condicionParaPasarLaLaguna tiro = (velocidad tiro > 80) && (altura tiro >= 1 && altura tiro <= 5)

-- Obstaculos: 

tunelConRampita :: Obstaculo
tunelConRampita tiro
                    | siLaPrecisionEsMayorA90 tiro = tiro {velocidad = 2*velocidad tiro, precision = 100, altura = 0}
                    | otherwise = tiroEnCero tiro

laguna :: LargoDeLaguna->Obstaculo
laguna largoDeLaguna tiro
            | condicionParaPasarLaLaguna tiro = tiro {altura = div (altura tiro) largoDeLaguna }
            | otherwise = tiroEnCero tiro

hoyo :: Obstaculo
hoyo tiro
        | condicionParaPasarHoyo tiro = tiroEnCero tiro
        | otherwise = tiroEnCero tiro

{-
a) Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo:
-}

--Agarro un tiro y le aplico la función obstuculo; si devolvio el tiro base (0,0,0), significa que no lo supero, entonces que devuelva True y no son iguales, ya que 
-- si no lo son, lo habra superado:

elTiroSuperaElObstaculo :: Tiro->Obstaculo->Bool
elTiroSuperaElObstaculo tiro obstaculo = not (obstaculo tiro == tirobase)

-- Yo aca lo que quiero hacer es agarrar un palo de la lista de palos, aplicarle el obstaculo al tiro que devuelven y si lo superan, que los coloque en una lista de "palos
-- utiles"

palosUtiles :: Jugador->Obstaculo->[Palo]
palosUtiles jugador obstaculo = filter (\unpalo obstaculo -> elTiroSuperaElObstaculo (unpalo (habilidad jugado) obstaculo)) palos

{-
b) Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo, 
el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.
BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.
-}

cuantosObstaculosPuedeSuperar :: [Obstaculo]->Tiro->Int
cuantosObstaculosPuedeSuperar listaDeObstaculos tiro = length (takeWhile (elTiroSuperaElObstaculo tiro) listaDeObstaculos)

{-
c) Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite 
superar más obstáculos con un solo tiro.
-}

--paloMasUtil:: Jugador->[Obstaculo]

{-
Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo,
se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un niño ganó 
el torneo si tiene más puntos que los otros niños.
-}

type JyP = (Jugador,Int)
jugador =fst
puntos = snd
listaDePuntos = map puntos
listaDeJugadores = map jugador

listaDePadres :: [JyP]-> [String]
listaDePadres lista = map padre (listaDeJugadores lista)

mayorCantidadDePuntos ::[JyP]->Int
mayorCantidadDePuntos listaDeJyP = maximum (listaDePuntos listaDeJyP) 

filtrarPerdedores :: [JyP]->[JyP]
filtrarPerdedores jyp = filter (\(_,puntos) -> puntos /= mayorCantidadDePuntos jyp) jyp 

padresDePerdedores :: [JyP]->[String]
padresDePerdedores lista = listaDePadres (filtrarPerdedores lista) 

listaJyP :: [JyP]
listaJyP = [(bart,150),(todd,100),(rafa,50)]

