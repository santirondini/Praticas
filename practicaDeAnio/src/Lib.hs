
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

type Evento = Ciudad -> Ciudad

transformacionSucesiva :: Float->Int->Evento
transformacionSucesiva aumento cantidadLetras unaciudad  = reevaluacion cantidadLetras (crisis (remodelacion aumento unaciudad))

crisis:: Evento
crisis unaciudad = unaciudad {atracciones = init (atracciones unaciudad), costoVida=costoVida unaciudad*0.9}

remodelacion :: Aumento -> Evento
remodelacion aumento unaciudad = unaciudad {nombre = "New " ++ nombre unaciudad, costoVida=costoVida unaciudad * (1 + aumento / 100)}

reevaluacion :: Int -> Evento
reevaluacion  cantidadLetras unaciudad 
    | ciudadSobria unaciudad cantidadLetras = unaciudad { costoVida = costoVida unaciudad * 1.1 }
    | otherwise = unaciudad { costoVida = costoVida unaciudad - 3 }

-- ============================================================================= Segunda parte del TP1:
 
data Anio = UnAnio{
    numero :: Int,
    eventos :: [Evento]
}

anio22 :: Anio
anio22 = UnAnio 2022 [remodelacion 5, crisis, reevaluacion 10]

anioEnCiudad :: Anio -> Evento
anioEnCiudad unAnio unaCiudad = foldl (\ciudad evento -> evento ciudad) unaCiudad (eventos unAnio)   
 

-- 4.1
ciudadPrueba :: Ciudad
ciudadPrueba = UnaCiudad {
    nombre = "Kentucky",
    fundacion = 1870,
    atracciones = ["Parque de diversiones", "Lago para pescar", "Shopping"],
    costoVida = 24500
}

{- 
4.2 Implementar una función que reciba una ciudad, un criterio de comparación y un evento,
 de manera que nos diga si la ciudad tras el evento subió respecto a ese criterio. 
-}
data Criterio = Nombre | Fundacion | Atracciones | CostoVida deriving Show

--criterio: nombre/fundacion/cantidad de atracciones/costo de vida

algoMejor :: Ciudad -> Evento -> Criterio -> Bool
--algoMejor ciudad evento criterio =  criterio ciudad > criterio (evento ciudad)
algoMejor ciudad evento Nombre = length (nombre ciudad) < length (nombre (evento ciudad))
algoMejor ciudad evento Fundacion = fundacion ciudad < fundacion (evento ciudad)
algoMejor ciudad evento Atracciones = length (atracciones ciudad) < length (atracciones (evento ciudad))
algoMejor ciudad evento CostoVida = costoVida ciudad < costoVida (evento ciudad)

{- 
4.3  Para un año, queremos aplicar sobre una ciudad solo los eventos que hagan que el costo de vida
suba. Debe quedar como resultado la ciudad afectada con dichos eventos.
-}

{- 
4.4 Para un año, queremos aplicar solo los eventos que hagan que el costo de vida baje. Debe 
quedar como resultado la ciudad afectada con dichos eventos.
-}

{- 
4.5 Para un año, queremos aplicar solo los eventos que hagan que el valor suba. Debe quedar como 
resultado la ciudad afectada con dichos eventos.
-}