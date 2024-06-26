import Data.Char (digitToInt)


type Actor = String
type Genero = String

data Pelicula = UnaPelicula {
    nombre :: String,
    actores :: [Actor],
    duracion :: Int,
    estreno :: Int
} deriving (Eq,Show)

taxidriver = UnaPelicula "Taxi Driver" ["De Niro", "Foster"] 113 1976
machete = UnaPelicula "Machete" ["De Niro", "Rodriguez"] 105 2010
harrypotter9 = UnaPelicula "Harry Potter 9" ["Watson","Radcliffe","Grint"] 1000 2022

--1) 

trabajoEn :: Pelicula->Actor->Bool
trabajoEn pelicula actor = actor `elem` actores pelicula

--2) talPeliculaEsDeTalGenero <=> los actores de la pelicula son del genero 

type Tupla = (Genero, [Actor])

todosLosActores :: [Tupla]
todosLosActores = [("comedia", ["Carrey", "Grint", "Stiller"]),("accion", ["Stallone", "Willis","Schwarzenegger"]), ("drama", ["De Niro", "Foster"])]

-- Ver especificación | Ejemplo: machete tiene De Niro y a Rodriguez, le pelicula deberia ser de drama ya que esta de Niro (No hace falta que este Rodriguez)


listaDeActoresSegunGenero :: Genero->[Actor]
listaDeActoresSegunGenero "comedia" = ["Carrey", "Grint", "Stiller"]
listaDeActoresSegunGenero "accion" = ["Stallone", "Willis","Schwarzenegger"]
listaDeActoresSegunGenero "drama" = ["De Niro", "Foster"]

elActorEsDelGenero :: Actor->Genero->Bool
elActorEsDelGenero actor genero = actor `elem` listaDeActoresSegunGenero genero

todosLosActoresDeLaPeliculaSonDelGenero :: [Actor]->Genero->Bool
todosLosActoresDeLaPeliculaSonDelGenero actores genero = all (`elActorEsDelGenero` genero) actores

algunoEsDelGenero :: [Actor]->Genero->Bool
algunoEsDelGenero actores genero = any (`elActorEsDelGenero` genero) actores

esDeTalGenero :: Pelicula->Genero->Bool
esDeTalGenero pelicula genero = todosLosActoresDeLaPeliculaSonDelGenero (actores pelicula) genero || algunoEsDelGenero (actores pelicula) genero

-- 3) 

type Premio = String

primeraPalabra :: String->String
primeraPalabra [] = []
primeraPalabra (x:xs)
                            | x /= ' ' = x : primeraPalabra xs
                            | otherwise = []


primerPalabraEsUnNumero :: String->Bool
primerPalabraEsUnNumero oracion = primeraPalabra oracion `elem` ["1","2","3","4","5","6","7","8","9","0"]

gano :: Pelicula->Premio->Bool
gano pelicula "Clasico setentista" = estreno pelicula >= 1970 && estreno pelicula <= 1979
gano pelicula "Plomo" = duracion pelicula > 120
gano pelicula premio
                    | primerPalabraEsUnNumero premio =  length (actores pelicula) == read (primeraPalabra premio)

{-
En el mundo hay festivales de cine que otorgan ciertos premios a las películas. Por ejemplo, Cannes otorga los premios 
3 son multitud y Clásico setentista. mientras que el festival de Berlín entrega los premios 4 son multitud, plomo y Clasico setentista.

a) Definir la función que permita averiguar cuántos premios de los que otorga un festival puede recibir una película 
dada y mostrar al menos dos ejemplos de cómo utilizarla para diferentes festivales. 

d) Representar un nuevo festival con al menos dos premios, en el que sea necesario utilizar composición de funciones

-}

cuantosPremiosOtorga :: Premio->Pelicula->[Premio]
cuantosPremiosOtorga "Cannes" pelicula = filter (gano pelicula) ["3 son multitud","Clasico setentista"]
cuantosPremiosOtorga "Berlín" pelicula = filter (gano pelicula) ["4 son multitud","Plomo","Clasico setentista"]






