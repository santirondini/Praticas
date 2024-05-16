import Distribution.Parsec.Warning (PWarnType(PWTExperimental))
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

dividirUniverso :: Universo->Universo
dividirUniverso universo = drop (div (length universo) 2)  universo

guanteleteCompleto :: Guantelete->Bool
guanteleteCompleto guantelete = (length (gemas guantelete) == 6) && (material guantelete == "uru")

chasquido :: Guantelete->Universo->Universo
chasquido guantelete universo
                            | guanteleteCompleto guantelete = dividirUniverso universo
                            | otherwise = universo

-- universo apto para pendex, si alguno de los personajes tiene menos 45

aptoParaPendex :: Universo->Bool
aptoParaPendex = any (\personaje -> edad personaje < 45)

-- energia total de un universo = sumatoria de enrgias de los integrantes que tienen mas de una habilidad

tienenMasDeUnaHabilidad :: Personaje->Bool
tienenMasDeUnaHabilidad personaje = length (habilidades personaje) > 1

listaDePersonajesConMasDeUnaHabilidad = filter tienenMasDeUnaHabilidad

energiaTotal :: Universo->Int
energiaTotal universo = sum (map energia (listaDePersonajesConMasDeUnaHabilidad universo))

mente :: Int->Gema
mente valor personaje = personaje {energia = energia personaje - valor}

restarDiezDeEnergia :: Personaje->Personaje
restarDiezDeEnergia p = p { energia = energia p - 10}

alma :: String->Gema
alma habilidad personaje
                        | habilidad `elem` habilidades personaje = restarDiezDeEnergia (personaje { habilidades = filter (/= habilidad) (habilidades personaje)})
                        | otherwise = restarDiezDeEnergia personaje

tieneDosHabilidadesOMenos :: Personaje->Bool
tieneDosHabilidadesOMenos personaje = length (habilidades personaje) <= 2

poder :: Gema
poder personaje
               | tieneDosHabilidadesOMenos personaje = personaje {habilidades = drop 2 (habilidades personaje), energia = 0}
               | otherwise = personaje {energia = 0}

tiempo :: Gema
tiempo personaje
                | div (edad personaje) 2 <= 18 = personaje
                | otherwise = personaje { edad = div (edad personaje) 2}

espacio :: String->Gema
espacio nuevoplaneta personaje = personaje { energia = energia personaje - 20, planeta = nuevoplaneta}

loca :: Gema->Personaje->Personaje
loca gema personaje = gema (gema personaje)

deGoma = UnGuantelete "goma" [tiempo, alma "usar Mjolnir", loca (alma "programacion en Haskell")]

utilizar:: Personaje->[Gema]->Personaje
utilizar = foldl (\personaje gema -> gema personaje)

--  

perdidaDeEnergia :: Personaje->Gema->Int
perdidaDeEnergia personaje gema = energia personaje - energia (gema personaje) 

mayorPerdidaDeEnergiaEntreDosGemas :: Personaje->Gema->Gema->Gema
mayorPerdidaDeEnergiaEntreDosGemas victima gema1 gema2 
                                        | perdidaDeEnergia (gema1 victima) > perdidaDeEnergia (gema2 victima) = gema1
                                        | otherwise = gema2 

gemaMasPoderosa :: Guantelete->Personaje->Gema
gemaMasPoderosa guantelete personaje = foldl1 (\gema1 gema2 -> mayorPerdidaDeEnergiaEntreDosGemas personaje gema1 gema2) (gemas guantelete)  







