
-- Vacaciones: 

data Turista = UnTurista {
    nivelDeCansancio :: Int,
    nivelDeEstres:: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
}deriving (Eq,Show)

type Excursion = Turista->Turista
type Paisaje = String
type Idioma = String
type Minutos = Int
type Marea = String

irALaPlaya :: Excursion
irALaPlaya turista
                  | viajaSolo turista = turista {nivelDeCansancio = nivelDeCansancio turista - 5}
                  | otherwise =  turista {nivelDeEstres = nivelDeEstres turista - 1}

apreciarPaisaje :: Paisaje->Excursion
apreciarPaisaje paisaje turista = turista {nivelDeEstres = nivelDeEstres turista - length paisaje}

salirHablandoIdioma :: Idioma->Excursion
salirHablandoIdioma idioma turista = turista {idiomas = idioma : idiomas turista, viajaSolo = False}

caminarXminutos :: Minutos->Excursion
caminarXminutos minutos turista = turista {nivelDeCansancio = nivelDeCansancio turista + div minutos 4, nivelDeEstres = nivelDeEstres turista - div minutos 4 }

paseoEnBarco :: Marea->Excursion
paseoEnBarco "fuerte" turista = turista {nivelDeCansancio = nivelDeCansancio turista + 10, nivelDeEstres = nivelDeEstres turista + 6}
paseoEnBarco "moderada" turista = turista
paseoEnBarco "tranquila" turista = salirHablandoIdioma "aleman" (apreciarPaisaje "mar" (caminarXminutos 10 turista))

-- Turistas:
ana = UnTurista 0 21 False ["español"]
beto = UnTurista 15 15 False ["aleman"]
cathi = UnTurista 15 15 False ["aleman","catalan"]

excursionesATurista :: Turista->[Excursion]->Turista
excursionesATurista = foldl (\turista excursion -> excursion turista)

reducirDiezPorcientoElEstres :: Excursion
reducirDiezPorcientoElEstres turista = turista {nivelDeEstres = nivelDeEstres turista - div (nivelDeEstres turista) 10}

hacerExcursion :: Turista->[Excursion]->Turista
hacerExcursion turista listaDeExcursiones = reducirDiezPorcientoElEstres (excursionesATurista turista listaDeExcursiones)

--Función dada por parcial:

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = String

deltaSegunT :: (Turista->Int)->Turista->Turista->Int
deltaSegunT f t1 t2 = f t1 - f t2

-- Entiendo la idea de que el algo2 sea el turista con la excursión aplicada pero no se porque no corre 

nDc :: Turista->Int
nDc = nivelDeCansancio

nDe ::  Turista->Int
nDe = nivelDeEstres

cDi :: Turista->Int
cDi t = length (idiomas t)

{-
deltaExcursionistasSegun :: Indice->Turista->Excursion->Int
deltaExcursionistasSegun "cansancio" turista excursion = deltaSegun (nDc (excursion turista) turista)
deltaExcursionistasSegun "estres" turista excursion = deltaSegun (nDe turista excursion)
deltaExcursionistasSegun "idiomas" turista excursion = deltaSegun (cdi turista excursion)
-}

-- El c se hace con la función anterior 

-- 3) tours => serie de excursiones 

type Tour = [Excursion]

completo :: [Excursion]
completo = [apreciarPaisaje "casacada", caminarXminutos 20, caminarXminutos 40, irALaPlaya, salirHablandoIdioma "melmacquiano"]

ladoB = [paseoEnBarco "tranquila", apreciarPaisaje "costa", caminarXminutos 120 ]

type ExcursionElegida = Excursion

viajesALaIslaVecina :: Marea->Tour->Tour
viajesALaIslaVecina "fuerte" tour = paseoEnBarco "fuerte" : tour ++ [apreciarPaisaje "lago", irALaPlaya] ++ [paseoEnBarco "fuerte"]
viajesALaIslaVecina "tranquila" tour = paseoEnBarco "tranquila" : tour ++ [paseoEnBarco "tranquila"]
viajesALaIslaVecina "moderada" tour = paseoEnBarco "moderada" : tour ++ [paseoEnBarco "moderada"]

hacerTourAIslaVecina :: Turista->Marea->Tour->Turista
hacerTourAIslaVecina turista marea tour = turistaConEstres (excursionesATurista turista (viajesALaIslaVecina marea tour)) tour

aplicarExcursionALadoB:: ExcursionElegida->Tour->Tour
aplicarExcursionALadoB excursionElegida tour = take 2 tour ++ [excursionElegida] ++ drop 1 tour

hacerTourLadoB :: Turista->Excursion->Turista
hacerTourLadoB turista excursionElegida = turistaConEstres (excursionesATurista turista (aplicarExcursionALadoB excursionElegida ladoB)) ladoB

hacerTourCompleto :: Turista->Turista
hacerTourCompleto turista  = turistaConEstres (excursionesATurista turista completo) completo

turistaConEstres :: Turista->Tour->Turista
turistaConEstres turista tour = turista {nivelDeEstres = nivelDeEstres turista + length tour}

-- tours convincentes: baja estres y sale acompañado. Las que bajan estres = ir a la playa, caminar y apreciar paisja. Se vuelve acompañado del salir hablando idioma

saleConMenosEstres :: Tour->Turista->Bool
saleConMenosEstres tour turista = nivelDeEstres turista - nivelDeEstres (excursionesATurista turista tour) < 0

saleAcompañado :: Tour->Turista->Bool
saleAcompañado tour turista = viajaSolo (excursionesATurista turista tour) /= viajaSolo turista

esConvincente :: [Tour]->Turista->Bool
esConvincente listaDeTours turista = any (\x-> saleConMenosEstres x turista) listaDeTours || any (\x -> saleAcompañado x turista ) listaDeTours

-- efectividad de un tour = sumatoria de la espiritualidad de cada turista a quienes les resulto convincente el tour 
-- espiritualidad = suma de perdidas de stres y cansancio 

turistasConvencidos :: Tour->[Turista]->[Turista]
turistasConvencidos tour turistas = filter (\turista -> esConvincente [tour] turista) turistas

perdidasDeCansancio :: Turista->Tour->Int
perdidasDeCansancio turista tour = abs (nivelDeCansancio turista - nivelDeCansancio (excursionesATurista turista tour))

perdidasDeEstres:: Turista->Tour->Int
perdidasDeEstres turista tour = abs (nivelDeEstres turista - nivelDeEstres (excursionesATurista turista tour))

espiritualidad :: Turista->Tour->Int
espiritualidad turista tour = perdidasDeCansancio turista tour + perdidasDeEstres turista tour

listaEspiritual :: Tour->[Turista]->[Int]
listaEspiritual tour = map (`espiritualidad` tour)

efectividadDelTour :: [Turista]->Tour->Int
efectividadDelTour turistas tour = sum (listaEspiritual tour (turistasConvencidos tour turistas))

-- 4) Contruir un tour donde se visten playas infinitas 

tourInfinito :: Tour
tourInfinito = irALaPlaya : [irALaPlaya]




