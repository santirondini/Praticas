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

remodelacion :: Ciudad->Aumento->Ciudad
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



 












