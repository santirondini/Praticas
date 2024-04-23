-- Ejercicio 2: Hechiceros 

-- Datos:

type Nombre = String
type Grado = Float
type Clan = String
type Antiguedad = Float
type Equipo = [Hechicero]

data Hechicero = UnHechicero {
    nombre :: Nombre,
    grado :: Grado,
    clan :: Clan,
    antiguedad :: Antiguedad
} deriving (Show)


-- Carga de Hechiceros 

nobara :: Hechicero
nobara = UnHechicero {
    nombre = "Nobara",
    grado = 3,
    clan = "Kugisaki",
    antiguedad = 1
}

maki :: Hechicero
maki = UnHechicero {
    nombre = "Maki",
    grado = 4,
    clan = "Zenin",
    antiguedad = 3
}

satoru :: Hechicero
satoru = UnHechicero {
    nombre = "Satoru",
    grado = 0,
    clan = "Gojo",
    antiguedad = 15
}

yuji :: Hechicero
yuji = UnHechicero {
    nombre = "Yuji",
    grado = 1,
    clan = "Itadori",
    antiguedad = 0
}

lista:: Equipo
lista =  [yuji, satoru, maki, nobara]

-- Hechicero con experiencia 

tieneExperiencia :: Hechicero->Bool
tieneExperiencia hechicero = antiguedad hechicero > 1

-- Equipos

estanPreparados :: Equipo->Bool
estanPreparados equipo = length equipo > 3

-- Subir de grado 

subirDeGrado :: Hechicero->Hechicero
subirDeGrado hechicero
                | suGradoEsCero hechicero = hechicero
                | otherwise = hechicero {grado = grado hechicero - 1}

suGradoEsCero :: Hechicero->Bool
suGradoEsCero hechicero = grado hechicero == 0

-- Prestigio 

esPrestigioso :: Hechicero->Bool
esPrestigioso hechiero = clan hechiero == "Zenin"
esPrestigioso hechiero = clan hechiero == "Gojo"
esPrestigioso hechiero = clan hechiero == "Kamo"

-- Invencibles 

tienenGradoCero :: Hechicero->Bool
tienenGradoCero x = grado x == 0

sonInvencibles :: Equipo->Bool
sonInvencibles = any tienenGradoCero

-- Grupo favorito

sonFavoritos :: Equipo->Bool
sonFavoritos = all esPrestigioso

-- Expertos

sonExpertos :: Equipo->Bool
sonExpertos = any tieneExperiencia

-- Grupo capaz

sonCapaces :: Equipo->Bool
sonCapaces equipo 
                | estanPreparados equipo = True
                | sonInvencibles equipo = True 
                | otherwise = False 
            
aumentarGrado :: Hechicero->Hechicero
aumentarGrado hechicero = hechicero {grado = grado hechicero + 1}

powerUp :: Equipo->Equipo
powerUp equipo 
            | sonCapaces equipo = map aumentarGrado equipo 
            | otherwise = equipo

-- Misiones: 

sigilo :: Hechicero->Float
sigilo hechicero = grado hechicero * 6

palabraMayor :: Hechicero->Char
palabraMayor hechicero = maximum (clan hechicero)

nivelTryhard :: Hechicero->Float
nivelTryhard hechicero = 1 / (grado hechicero + 1)

nivelBurocratico :: Hechicero->Int
nivelBurocratico hechicero = length (clan hechicero) 

