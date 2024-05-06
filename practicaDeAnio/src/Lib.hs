import GHC.Exts.Heap.Closures (GenClosure(fun))
type Valor = Float
type Nombre = String
type Fundacion = Float
type Atraccion = String
type CostoVida = Float
type Aumento = Float
type CantidadDeLetras = Int

data Ciudad =   UnaCiudad{
    nombre :: Nombre,
    fundacion :: Fundacion,
    atracciones :: [Atraccion],
    costoVida :: CostoVida
}deriving (Show,Eq,Ord)

-- Primera parte del TP1:

calcularValor :: Ciudad->Valor
calcularValor unaciudad
    |fundacion unaciudad < 1800 = (1800 - fundacion unaciudad) * 5
    |null (atracciones unaciudad) = costoVida unaciudad * 2
    |otherwise = costoVida unaciudad * 3


tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada unaciudad = any (\atraccion -> head atraccion == 'A' || head atraccion == 'E' || head atraccion == 'I' || head atraccion == 'O' || head atraccion == 'U') (atracciones unaciudad)

ciudadSobria:: Ciudad -> Int -> Bool
ciudadSobria unaciudad cantidadLetras = not (any (\atraccion -> length atraccion <= cantidadLetras) (atracciones unaciudad))

ciudadRara:: Ciudad -> Bool
ciudadRara unaciudad = length (nombre unaciudad) < 5

agregarAtraccion:: Ciudad-> String -> Ciudad
agregarAtraccion unaciudad nuevaatraccion = unaciudad { atracciones = nuevaatraccion : atracciones unaciudad, costoVida = costoVida unaciudad * 1.2}

crisis:: Ciudad -> Ciudad
crisis unaciudad = unaciudad {atracciones = init (atracciones unaciudad), costoVida=costoVida unaciudad*0.9}

remodelacion:: Ciudad->Aumento->Ciudad
remodelacion unaciudad aumento = unaciudad {nombre = "New " ++ nombre unaciudad, costoVida=costoVida unaciudad * (1 + aumento / 100)}

reevaluacion :: Ciudad -> Int -> Ciudad
reevaluacion unaciudad cantidadLetras
    | ciudadSobria unaciudad cantidadLetras = unaciudad { costoVida = costoVida unaciudad * 1.1 }
    | otherwise = unaciudad { costoVida = costoVida unaciudad - 3 }


transformacionSucesiva :: Ciudad->Float->Int->Ciudad
transformacionSucesiva unaciudad aumento = reevaluacion (crisis (remodelacion unaciudad aumento))

-- ============================================================================= Segunda parte del TP1:
{- 
Mi forma: Por santino julian RONINI AKAK EL SANEEEEEEEEEEEEEEE
-}

type FuncionRemodelacion = Ciudad->Aumento->Ciudad 
type Crisis = (Bool,Ciudad->Ciudad)
type Remodelacion = (Bool,Ciudad->Aumento->Ciudad,Aumento)
type Reevaluacion = (Bool,Ciudad->Int->Ciudad,Int)

{-
boleano = fst
funcion = snd
-}

--Accesos:

booleano = fst
eventoC = snd

aumenlet (_, _, num) = num
booleano3 (tf, _, _) = tf
eventoT (_,ev,_) = ev

--aumenlet: aumento/cantidad de letras
--booleano3: es como first para una tupla de 3 elementos

--Cambiar el año:

data Anio = UnAnio {
    numero :: Int,
    tieneCrisis :: Crisis,
    tieneRemodelacion :: Remodelacion,
    tieneReevaluacion :: Reevaluacion
}



ciudadPrueba :: Ciudad
ciudadPrueba = UnaCiudad {
    nombre = "Kentucky",
    fundacion = 1870,
    atracciones = ["Parque de diversiones", "Lago para pescar", "Shopping"],
    costoVida = 24500
}

anioPrueba :: Anio
anioPrueba = UnAnio {
    numero = 2022,
    tieneCrisis = (True, crisis),
    tieneRemodelacion = (True, remodelacion, 5.0),
    tieneReevaluacion = (True, reevaluacion, 7)
}



hayCrisisEn:: Anio->Bool
hayCrisisEn = booleano.tieneCrisis

hayRemodelacionEn :: Anio->Bool
hayRemodelacionEn = booleano3.tieneRemodelacion

hayReevaluacionEn :: Anio->Bool
hayReevaluacionEn = booleano3.tieneReevaluacion

-- Remodelación:

numeroDeAnio :: Anio->Int
numeroDeAnio  = numero 

aumentoDeAnio :: Anio->Aumento
aumentoDeAnio anio = aumenlet (tieneRemodelacion anio) 

funcionRemodelacion :: Anio->FuncionRemodelacion
funcionRemodelacion anio = eventoT (tieneRemodelacion anio)

aplicarRemoelacionDeAnioEnCiudad:: Anio->Ciudad->Ciudad
aplicarRemoelacionDeAnioEnCiudad anio ciudad =    

























