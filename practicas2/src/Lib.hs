-- Ejercicio: Fuente de los deseos

-- Variables predefinidas:

type Sueños = [String]
type Coeficiente = Int
type Edad = Int
type Nombre = String
type Felicidonios = Int

data Persona = UnaPersona {
    edad :: Edad,
    nombre :: Nombre,
    felicidonios :: Felicidonios,
    sueños :: Sueños
} deriving (Show)

-- Persona prueba:

juan :: Persona
juan = UnaPersona {
    nombre = "Juan Carlos Alberto",
    edad = 25,
    felicidonios = 180,
    sueños = ["Recibirse", "Dormir", "Jugar en San Lorenzo"]
}

-- Coeficiente de Satisfaccion: 

cantidadDeSueños :: Sueños->Int
cantidadDeSueños = length

coeficienteDeSatisfaccion :: Persona->Coeficiente
coeficienteDeSatisfaccion persona
                         |  felicidonios persona > 100 = feliz persona
                         |  felicidonios persona < 100 && felicidonios persona > 50 = cantidadDeSueños (sueños persona) * felicidonios persona
                         |  otherwise = 40

feliz :: Persona->Coeficiente
feliz persona = felicidonios persona * edad persona

-- Nombre largo:

nombreLargo :: Persona->Bool
nombreLargo persona = length (nombre persona) > 10

-- Persona suertuda:

esSuertuda :: Persona->Bool
esSuertuda persona = even (3 * coeficienteDeSatisfaccion persona)

-- Nombre Lindo:

tieneNombreLindo :: Persona->Bool
tieneNombreLindo persona = head (nombre persona) == 'L'

-- Cumplir Sueños:

multiplicacionPrevia :: Persona->Int
multiplicacionPrevia persona = coeficienteDeSatisfaccion persona * cantidadDeSueños (sueños persona) 

cumpleSusSueños :: Persona->Persona
cumpleSusSueños persona = persona { felicidonios = felicidonios persona + multiplicacionPrevia persona, sueños = []}

-- Fuente de los Deseos 


agregarDiezFelicidonios :: Persona->Persona
agregarDiezFelicidonios persona = persona {felicidonios = felicidonios persona + 10}

tirarMoneda :: Persona->Persona
tirarMoneda persona = agregarDiezFelicidonios (siEsSuertuda persona)

siEsSuertuda :: Persona->Persona
siEsSuertuda persona
                    | esSuertuda persona = cumpleSusSueños persona 
                    | otherwise = persona 