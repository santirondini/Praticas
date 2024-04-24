-- Ejercicio: Elenco 

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

-- 1)

tuvoValoracion :: Persona->Bool
tuvoValoracion actor =  masDeTres (actuaciones actor)

masDeTres :: [Actuacion]->Bool 
masDeTres lista = valoracion (last lista) > 3

peliculaConOscar :: [Pelicula]->Persona->Bool
pelicula ganadorasDeOscar actor = any (x == primeraPelicula actor)() 

primerPelicula :: Persona->Pelicula
primerPelicula actor = head (actuaciones actor)



oscar :: Persona->Bool
oscar actor
            | recibioOscar actor 
            | 