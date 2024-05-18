import Data.List (sort)
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

--2)

type TiroResultante = Tiro

golpe :: Jugador->Palo->TiroResultante
golpe jugador palo = palo (habilidad jugador)

-- 3) 


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

-- 4) 

elTiroSuperaElObstaculo :: Tiro->Obstaculo->Bool
elTiroSuperaElObstaculo tiro obstaculo = not (obstaculo tiro == tirobase)

palosUtiles :: Jugador->Obstaculo->[Palo]
palosUtiles jugador obstaculo = filter (\p -> elTiroSuperaElObstaculo (p (habilidad jugador)) obstaculo) palos

cuantosObstaculosSeguidosPuedeSuperar :: [Obstaculo]->Tiro->Int
cuantosObstaculosSeguidosPuedeSuperar listaDeObstaculos tiro = length (takeWhile (elTiroSuperaElObstaculo tiro) listaDeObstaculos)

{-
c) Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite 
superar más obstáculos con un solo tiro. 

Tanto en este ejercicio como en el de Parcial4, pongo mal la semilla
-}

paloMasUtilEntreDos :: Palo->Palo->Jugador->[Obstaculo]->Palo
paloMasUtilEntreDos palo1 palo2 jugador obstaculos 
                                          | cuantosObstaculosSeguidosPuedeSuperar obstaculos (palo1 (habilidad jugador)) > cuantosObstaculosSeguidosPuedeSuperar obstaculos (palo2 (habilidad jugador)) = palo1 
                                          | otherwise = palo2

type CantidadDeObstaculos = Int

paloMasUtil:: Jugador->[Obstaculo]->Palo
paloMasUtil jugador obstaculos = foldl1 (\p1 p2 -> paloMasUtilEntreDos p1 p2 jugador obstaculos) palos   
<<<<<<< HEAD

=======
>>>>>>> 3282d1fb1d2b58d08cda96cfd6bf870bde916eff

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

