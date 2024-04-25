
-- Ejercicio: Elenco 

type Valor = Int
type Pelicula = String

data Persona = UnaPersona {
    nombre :: String,
    recibioOscar :: Bool,
    actuaciones :: [Actuacion]
} deriving (Show)

data Actuacion = UnaActuacion {
    pelicula :: Pelicula,
    valoracion :: Int
} deriving (Show)

actuacion4 :: Actuacion
actuacion4 = UnaActuacion {
    pelicula = "La La Land",
    valoracion = 2
}

actuacion1  :: Actuacion
actuacion1 = UnaActuacion {
    pelicula = "Titanic",
    valoracion = 8
}

actuacion2 :: Actuacion
actuacion2 = UnaActuacion {
    pelicula = "El padrino II",
    valoracion = 5
}

actuacion3 :: Actuacion
actuacion3 = UnaActuacion {
    pelicula = "Spiderman 3",
    valoracion = 7
}

actor :: Persona
actor = UnaPersona{
    nombre = "Miguel",
    recibioOscar = True,
    actuaciones = listaDeActuacion
}

listaDeActuacion :: [Actuacion]
listaDeActuacion = [actuacion1, actuacion2, actuacion3, actuacion4]

peliculasPremiadas :: [Pelicula]
peliculasPremiadas = ["Titanic", "La La Land", "500 dias con ella ", "El Padrino"]

-- Primer fila:

-- 1)
tuvoValoracion :: Persona->Bool
tuvoValoracion actor =  masDeTres (actuaciones actor)

masDeTres :: [Actuacion]->Bool
masDeTres lista = valoracion (last lista) > 3

-- 2)

sacarPrimero :: Persona->Pelicula
sacarPrimero actor = pelicula (head (actuaciones actor))

primeraPeliculaConOscar :: Persona->Bool
primeraPeliculaConOscar actor = sacarPrimero actor `elem` peliculasPremiadas

tieneOscar :: Persona->Bool
tieneOscar = recibioOscar

recibioOscaroPrimeraPeliConOscar :: Persona->Bool
recibioOscaroPrimeraPeliConOscar actor = primeraPeliculaConOscar actor || tieneOscar actor

--3)

cantidadDePeliculas :: Persona->Int
cantidadDePeliculas actor = length (actuaciones actor)

tieneExperiencia :: Persona->Valor->Bool
tieneExperiencia actor numeroDePeliculas = cantidadDePeliculas actor > numeroDePeliculas

-- 4)

hizoDeclaracionesDesafortunadas :: Persona->Persona
hizoDeclaracionesDesafortunadas actor = actor {recibioOscar = False}

{-

5) Si un actor/actriz tiene éxito en su última actuación (una valoración mayor a 3 estrellas) 
lo contratan para la parte 2 de la pelicula, pero recibe una valoración 2 unidades menor.

Preguntar: 
-}

ultimaActuacion:: Persona->Actuacion
ultimaActuacion actor = last (actuaciones actor)

ultimaPeliculaConValoracionMayorATres :: Persona->Bool
ultimaPeliculaConValoracionMayorATres actor = valoracion (ultimaActuacion actor) > 3

restarDosAValoracionDeActuacion :: Actuacion->Actuacion
restarDosAValoracionDeActuacion actuacion = actuacion {valoracion = valoracion actuacion - 2}

cambiarUltimoDeLista :: [Actuacion]->[Actuacion]
cambiarUltimoDeLista lista = restarDosAValoracionDeActuacion (last lista) 

exitoEnSuUltimaActuacion :: Persona->Persona
exitoEnSuUltimaActuacion actor
                              | ultimaPeliculaConValoracionMayorATres actor = cambiarUltimoDeLista (actuaciones actor) 
                              | otherwise = actor


{-
6)  Determinar si el/la actor/actriz siempre tuvo valoración de 3 estrellas o
más en todas las películas en las que actuó, y actuó en más de una película. 
 
Preguntar:

-}
buenaValoracionEnTodasYMasDeUnaPelicula :: Persona->Bool
buenaValoracionEnTodasYMasDeUnaPelicula actor = buenaValoracionEnTodasLasPeliculas (actor) && length (actuaciones actor) > 1

buenaValoracionEnTodasLasPeliculas :: Persona->Bool 
buenaValoracionEnTodasLasPeliculas actor = all (>3) (valoracion (actuaciones actor))


