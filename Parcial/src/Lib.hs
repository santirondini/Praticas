data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distancia:: Int
}deriving(Show,Ord,Eq)

type Carrera = [Auto]

-- 1 a)

autosDistintos :: Auto->Auto->Bool
autosDistintos auto1 auto2 = (color auto1 /= color auto2) && (distancia auto1 /= distancia auto2) && (velocidad auto1 /= velocidad auto2)

distanciaMenorADiez :: Auto->Auto->Bool
distanciaMenorADiez auto1 auto2 = abs (distancia auto1 - distancia auto2) < 10

estanCerca :: Auto->Auto->Bool
estanCerca auto1 auto2 = autosDistintos auto1 auto2 && distanciaMenorADiez auto1 auto2

-- 1 b) autotraquilo <=> no tiene autos cerca y esta prinmero (distancia mayor a la de los demas )

noTieneAutosCerca :: Carrera->Auto->Bool
noTieneAutosCerca carrera auto = not (all (estanCerca auto) carrera)

distanciaMayor :: Auto->Auto->Bool
distanciaMayor a1 a2 = distancia a1 >= distancia a2

estaPrimero :: Carrera->Auto->Bool
estaPrimero carrera auto = all (distanciaMayor auto) carrera

estaTranquilo :: Auto->Carrera->Bool
estaTranquilo auto carrera = estaPrimero carrera auto && noTieneAutosCerca carrera auto

-- 1 c) puesto de un auto = 1 + cantidad de autos que le van ganando

puestoDelAuto :: Carrera->Auto->Int
puestoDelAuto carrera auto =  length (drop (length (takeWhile (/=auto) carrera) + 1) carrera) + 1

-- 2 a) auto corra = x(t) = v*t + d

type Tiempo = Int

corre :: Auto->Tiempo->Auto
corre auto tiempo = auto {distancia = distancia auto + tiempo*velocidad auto}

-- 2 b) i

type Modificador = Int->Int

alterarVelocidad :: Modificador->Auto->Auto
alterarVelocidad modificador auto = auto {velocidad = modificador (velocidad auto) }

--2 b) ii 
bajarVelocidad :: Modificador->Auto->Auto
bajarVelocidad modificador auto
                               | velocidad auto - modificador (velocidad auto) <= 0 = auto {velocidad = 0}
                               | otherwise  = auto {velocidad = velocidad auto - modificador (velocidad auto)}

{- 
3) powerups ==> impactan el estado general de la carrera
terremoto = los autos cerca de quien gatilló el power up, bajan su velocidad en 50
miguelito = se le pasa un valor int (cuanto deberan bajar la velocidad los autos). Los autos afectados son aquellos que vayan por delante de quien ejecuto el p.u
jetpack = afecta a solo quien gatilla el poder. Tiene tiempo limitado el cual se condigura. El auto duplica su velocidad, corre durante el tiempo indicado y vuelve
a su velocidad pre powerup. 
-}

-- Preguntar si es necesario usarla: 

afectarALosQueCumplen :: (a->Bool) -> (a->a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- Terremoto: 

autosCercanos :: Auto->Carrera->[Auto]
autosCercanos auto = filter (estanCerca auto)

bajarVelocidadSegunValor :: Int->Auto->Auto
bajarVelocidadSegunValor valor auto
                                | velocidad auto - valor <= 0 = auto {velocidad = 0}
                                | otherwise = auto {velocidad = velocidad auto - valor}

terremoto :: Auto->Carrera->Carrera
terremoto auto carrera = map (bajarVelocidadSegunValor 50) (autosCercanos auto carrera)

-- Miguelito:

type Valor = Int

autosPorDelante :: Auto->Carrera->Carrera
autosPorDelante auto carrera = drop (length (takeWhile (/= auto) carrera) + 1) carrera

miguelito :: Valor->Auto->Carrera->Carrera
miguelito valor auto carrera = map (bajarVelocidadSegunValor valor) (autosPorDelante auto carrera)

-- Jetpack:

jetpack :: Auto->Tiempo->Auto
jetpack auto tiempo = auto {distancia = distancia auto + tiempo* (2*velocidad auto) }

-- 4 a) lista de eventos que afectan la carrera. 

rojo = UnAuto {color = "Rojo", velocidad = 120, distancia=0}
blanco = UnAuto {color = "Blanco", velocidad = 120, distancia=0}
azul = UnAuto {color = "Azul", velocidad = 120, distancia=0}
negro = UnAuto {color = "Negro", velocidad = 120, distancia=0}
formula1 = [rojo,blanco,azul,negro]

type Color = String
type Evento = Carrera->Carrera
type Posiciones = (Int,Color)

aplicarListaDeEventosACarrera :: Carrera->[Evento]->Carrera
aplicarListaDeEventosACarrera = foldl (\carrera evento -> evento carrera)

sacarPosicion :: Carrera->Auto->Posiciones
sacarPosicion carrera auto = (puestoDelAuto carrera auto, color auto)

armarPosiciones :: Carrera->[Posiciones]
armarPosiciones carrera = map (sacarPosicion carrera) (reverse carrera)

simularCarrera :: Carrera->[Evento]->[Posiciones]
simularCarrera carrera listaDeEventos = armarPosiciones (aplicarListaDeEventosACarrera carrera listaDeEventos)

{- 
correnTodos = todos los autos participantes corren durante un determinado tiempo
usaPowerUp =  que a partir de un power up y del color del auto que gatilló el poder en cuestión, 
encuentre el auto correspondiente dentro del estado actual de la carrera para usarlo y produzca los efectos esperados para ese power up.

No entendí la consigna 
-}

type PowerUp = Auto->Carrera

correnTodos :: Tiempo->Evento
correnTodos tiempo = map (`corre` tiempo)

-- 4 c) ¿Como meto el jetpack? Si el jetpack va de auto y tiempo a auto ¿Cómo podria transformarlo en Evento?

simulacion :: [Evento]
simulacion = [correnTodos 30, terremoto blanco, correnTodos 40, miguelito 20 blanco, correnTodos 10]





