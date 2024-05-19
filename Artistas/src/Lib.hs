type Cancion = String

data Artista = UnArtista {
    nombre :: String,
    canciones :: [Cancion]
} deriving Show

fito :: Artista
fito = UnArtista "Fito Paez" ["11 y 6", "El amor despues del amor", "Mariposa Tecknicolor"]

calamaro :: Artista
calamaro = UnArtista "Andres Calamaro" ["Flaca", "Sin Documentos", "Tuyo siempre"]

paty :: Artista
paty = UnArtista "Taylor Paty" ["Shake It Off", "Lover"]

-- 1)

distintoAUnNumero :: Char->Bool
distintoAUnNumero caracter =  caracter /= '1' && caracter /= '2' && caracter /= '3' && caracter /= '4' && caracter /= '5' && caracter /= '6' && caracter /= '7' && caracter /= '8' && caracter /= '9'

letrasSignificativas :: [Char]->Int
letrasSignificativas [] = 0
letrasSignificativas (x:xs)
                            | x /= ' ' || distintoAUnNumero x = 1 + letrasSignificativas xs
                            | otherwise = letrasSignificativas xs


calificacionDeUnaCancion :: Cancion->Int
calificacionDeUnaCancion cancion = letrasSignificativas cancion + 10

-- 2) artista exitoso: suma de las calificaciones > 20 es mayor a 50

calificacionesMayoresA20:: [Cancion]->[Cancion]
calificacionesMayoresA20 =  filter (\cancion -> calificacionDeUnaCancion cancion > 20)

calificacionesBuenas = map calificacionDeUnaCancion

esExitoso :: Artista->Bool
esExitoso artista = sum (calificacionesBuenas (calificacionesMayoresA20 (canciones artista))) > 50

-- 3) 

artistasExitosos :: [Artista]->[Artista]
artistasExitosos = filter esExitoso



