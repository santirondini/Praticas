
type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
 ambientes :: Int,
 superficie :: Int,
 precio :: Int,
 barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
   mail :: Mail,
   busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
 (ordenarSegun criterio . filter (not . criterio x)) xs ++
 [x] ++
 (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

deptos = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]



{-

Se pide desarrollar las siguientes funciones y consultas de modo que se aprovechen tanto como sea posible los conceptos de orden superior, aplicación parcial y composición.

Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar
esa función sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.
Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.

-}

mayor :: (a->Int) -> a -> a -> Bool
mayor f a b
           | f a > f b = True
           | otherwise = False

menor ::  (a->Int) -> a -> a -> Bool
menor f a b
           | f a < f b = True
           | otherwise = False

{-
ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra
en alguno de los barrios de la lista.

cumpleRango que a partir de una función y dos números, indique si el valor retornado por
la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.
-}

ubicadoEn :: [Barrio]->Requisito
ubicadoEn barrios departamento = barrio departamento `elem` barrios

cumpleRango :: Depto->(Depto->Int) -> Int -> Int -> Bool
cumpleRango depto f n1 n2
                        | (n1 /= n2) && ((f depto > n1 && f depto < n2) || (f depto < n1 && f depto > n2)) = True
                        | otherwise = False

{-
Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.

Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una lista de departamentos 
retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido.

Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con:

Encontrarse en Recoleta o Palermo
Ser de 1 o 2 ambientes
Alquilarse a menos de $6000 por mes
-}

type Criterio = Depto->Depto->Bool

cumpleBusqueda :: Busqueda->Depto->Bool
cumpleBusqueda requisitos depto = all (\requisito -> requisito depto) requisitos

cumplenConLaBusqueda :: [Depto]->Busqueda->[Depto]
cumplenConLaBusqueda departamentos requisitos = filter (cumpleBusqueda requisitos) departamentos

cumplen :: Busqueda->Criterio->[Depto]->[Depto]
cumplen requisitos criterio departamentos = ordenarSegun criterio (cumplenConLaBusqueda departamentos requisitos)

-- Ejemplo 

recoletaOpalermo :: Requisito
recoletaOpalermo departamento = barrio departamento == "Recoleta" || barrio departamento == "Palermo"

unoOdosAmbientes :: Requisito
unoOdosAmbientes departamento = ambientes departamento == 1 || ambientes departamento == 2

menosDe6000 :: Requisito
menosDe6000 departamento = precio departamento < 6000

busqueda :: Busqueda
busqueda = [recoletaOpalermo,menosDe6000,unoOdosAmbientes]

mayorSuperficie :: Criterio
mayorSuperficie = mayor superficie

{-
Definir la función mailsDePersonasInteresadas que a partir de un departamento y una 
lista de personas retorne los mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado.
-}








