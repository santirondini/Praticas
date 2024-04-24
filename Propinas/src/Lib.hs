
-- Ejercicio 3: Propinas 

-- Datos:

type Nombre = String
type Precio = Float
type NombreB = String
type Plato = (Comida,Bebida)
type Menu = [Plato]
type Propina = Float


data Comida = UnaComida {
    nombreDeComida :: Nombre,
    precio :: Precio
} deriving (Show)

data Bebida = UnaBebida {
    nombreDeBebida :: NombreB,
    precioB :: Precio
} deriving (Show)

-- Menú 

fideos :: Comida
fideos = UnaComida "Fideos" 4000

milanesa :: Comida
milanesa = UnaComida "Milanesa" 3200

{-bife :: Comida
bife = UnaComida "Bife" 4750

ensalada :: Comida
ensalada = UnaComida "Ensalada" 2500
-}
cocacola :: Bebida
cocacola = UnaBebida "Coca-Cola" 1800

cerveza :: Bebida
cerveza = UnaBebida "Cerveza" 2900

--agua :: Bebida
--agua = UnaBebida "Agua" 1350

plato1 :: Plato
plato1 = (fideos,cocacola)

plato2 :: Plato
plato2 = (fideos,cerveza)

plato3 :: Plato
plato3 = (milanesa,cocacola)

plato4 :: Plato
plato4 = (milanesa,cerveza)

menu :: Menu
menu = [plato1, plato2, plato3, plato4]

comida = fst
bebidaa = snd

calcularValorDelPlato :: Plato->Precio
calcularValorDelPlato plato = precio (comida plato) + precioB (bebidaa plato)

dejarPropinaDiezPorciento :: Plato->Propina
dejarPropinaDiezPorciento plato = calcularValorDelPlato plato + calcularValorDelPlato plato / 10

precioMayorADosMill :: Plato->Bool
precioMayorADosMill plato = calcularValorDelPlato plato > 2000

cienOCincuenta :: Plato->Float
cienOCincuenta plato
                  | precioMayorADosMill plato = calcularValorDelPlato plato + 50
                  | otherwise = calcularValorDelPlato plato + 100

propinaDeCincoPorciento :: Plato->Propina
propinaDeCincoPorciento plato = calcularValorDelPlato plato / 20

nombresDePropinas :: String->Bool
nombresDePropinas "cinco" = True 
nombresDePropinas "cero" = True
nombresDePropinas "diez" = True
nombresDePropinas "cinco" = True

operacionParaDejarPropina :: Plato->String->Float
operacionParaDejarPropina p propina
                            | nombresDePropinas propina = propinaDeCincoPorciento p + calcularValorDelPlato p 
                            | nombresDePropinas propina = calcularValorDelPlato p 
                            | nombresDePropinas propina = dejarPropinaDiezPorciento p 
                            | nombresDePropinas propina = calcularValorDelPlato p 



