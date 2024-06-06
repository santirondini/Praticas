import Data.Fixed (Uni)
import GHC.RTS.Flags (ParFlags(migrate))

--Escuelita de Thanos 

data Personaje = UnPersonaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
} deriving (Eq,Show)

data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gema]
}

type Universo = [Personaje]
type Gema = Personaje->Personaje

--Personajes Prueba:

ironman :: Personaje
ironman = UnPersonaje 50 10 ["plata"] "tony" "tierra"

hulk :: Personaje
hulk = UnPersonaje 45 50 ["ponerse verde", "crecer 20 metros"] "bruce" "tierra"

spiderman :: Personaje
spiderman = UnPersonaje 20 25 ["sentido aracnido", "inteligencia", "tia may"] "peter" "tierra"

mcu = [ironman,hulk,spiderman]

mitadDelUniverso :: Universo -> Int
mitadDelUniverso universo = div (length universo) 2

dividirUniverso :: Universo->Universo
dividirUniverso universo = drop (mitadDelUniverso universo) universo

cantidadDeGemasIgualA :: Int -> Guantelete -> Bool
cantidadDeGemasIgualA cantidad guantelete = length (gemas guantelete) == cantidad

hechoDeUru :: Guantelete -> Bool
hechoDeUru guantelete = material guantelete == "uru"

guanteleteCompleto :: Guantelete->Bool
guanteleteCompleto guantelete = cantidadDeGemasIgualA 6 guantelete && hechoDeUru guantelete

chasquido :: Guantelete->Universo->Universo
chasquido guantelete universo
                            | guanteleteCompleto guantelete = dividirUniverso universo
                            | otherwise = universo

-- universo apto para pendex, si alguno de los personajes tiene menos 45

esJoven :: Personaje -> Bool
esJoven personaje = edad personaje < 45

aptoParaPendex :: Universo->Bool
aptoParaPendex = any esJoven

-- energia total de un universo = sumatoria de enrgias de los integrantes que tienen mas de una habilidad

tienenMasDeUnaHabilidad :: Personaje->Bool
tienenMasDeUnaHabilidad personaje = length (habilidades personaje) > 1

listaDePersonajesConMasDeUnaHabilidad = filter tienenMasDeUnaHabilidad

energiaTotal :: Universo->Int
energiaTotal universo = sum (map energia (listaDePersonajesConMasDeUnaHabilidad universo))

resta :: Int -> Gema
resta valor personaje = personaje {energia = energia personaje - valor }

tieneXhabilidad :: String -> Personaje -> Bool
tieneXhabilidad habilidad personaje = habilidad `elem` habilidades personaje  
  
sacarHabilidad :: String -> Personaje -> Personaje
sacarHabilidad habilidad personaje = personaje {habilidades = filter (/= habilidad) (habilidades personaje)}

mente :: Int->Gema
mente = resta

alma :: String->Gema
alma habilidad = resta 10.sacarHabilidad habilidad  

tieneDosHabilidadesOMenos :: Personaje->Bool
tieneDosHabilidadesOMenos personaje = length (habilidades personaje) <= 2

sacarDosHabilidades :: Gema
sacarDosHabilidades personaje = personaje {habilidades = drop 2 (habilidades personaje)}

poder :: Gema
poder personaje = resta (energia personaje) (sacarDosHabilidades personaje)

conMitadDeSuEdad :: Gema
conMitadDeSuEdad p = p {edad = div (edad p) 2}

edadAlaMitad :: Gema
edadAlaMitad p  
                | div (edad p) 2 < 18 = p
                | otherwise = conMitadDeSuEdad p

tiempo :: Gema
tiempo = edadAlaMitad 

transportarA :: String -> Gema
transportarA nuevo p = p {planeta = nuevo}

espacio :: String -> Gema
espacio nuevo = resta 20 . transportarA nuevo 

loca :: Gema->Personaje->Personaje
loca gema = gema.gema 

deGoma = UnGuantelete "goma" [tiempo, alma "usar Mjolnir", loca (alma "programacion en Haskell")]

utilizar:: Personaje->[Gema]->Personaje
utilizar = foldl (\personaje gema -> gema personaje)

-- 6) 

perdidaDeEnergia :: Personaje->Gema->Int
perdidaDeEnergia personaje gema = energia personaje - energia (gema personaje)

mayorPerdidaDeEnergiaEntreDosGemas :: Personaje->Gema->Gema->Gema
mayorPerdidaDeEnergiaEntreDosGemas victima gema1 gema2
                                        | perdidaDeEnergia victima gema1 > perdidaDeEnergia victima gema2 = gema1
                                        | otherwise = gema2

gemaMasPoderosa :: Guantelete->Personaje->Gema
gemaMasPoderosa guantelete personaje = foldl1 (\gema1 gema2 -> mayorPerdidaDeEnergiaEntreDosGemas personaje gema1 gema2) (gemas guantelete)

-- 7) Dado: 

infinitasGemas :: Gema->[Gema]
infinitasGemas gema = gema : infinitasGemas gema

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete personaje = utilizar personaje (take 3 (gemas guantelete))








