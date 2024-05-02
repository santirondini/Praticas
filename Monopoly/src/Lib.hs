
{-

Consultar: El hecho de que tengan una lista de funciones en el data, ¿Garantiza que esa función se va a efectuar en el participante?

-}

type Accion = Participante->Participante
type Nombre = String
type Dinero = Float
type Tactica = String

data Propiedades = UnaPropiedad { nombreDePropiedad :: Nombre, precio :: Dinero }

data Participante = UnParticipante {
    nombreDeParticipante :: Nombre,
    cantidadDeDinero :: Dinero,
    tactica :: Tactica,
    acciones :: [Accion],
    propiedades :: [Propiedades]
}

--Participantes: 

carolina :: Participante
carolina = UnParticipante {
    nombreDeParticipante = "Carolina",
    cantidadDeDinero = 500,
    tactica = "Accionista",
    acciones = [pasarPorElBanco],
    propiedades =[]
}

manuel :: Participante
manuel = UnParticipante {
     nombreDeParticipante = "Manuel",
    cantidadDeDinero = 500,
    tactica = "Oferente singular",
    acciones = [enojarse, pasarPorElBanco],
    propiedades =[]
}


-- ACCIONES: 

--Cobrar Alquileres:

cobrarAlquileres :: Accion
cobrarAlquileres participante = participante { propiedades = map accionarEnPropiedad (propiedades participante)}

accionarEnPropiedad :: Propiedades->Propiedades
accionarEnPropiedad propiedad
                            | esBarata propiedad = sumarDiezAPropiedad propiedad
                            | otherwise = sumarVeinteAPropiedad propiedad

esBarata :: Propiedades->Bool
esBarata propiedad = precio propiedad < 150

sumarDiezAPropiedad:: Propiedades->Propiedades
sumarDiezAPropiedad propiedad = propiedad {precio = precio propiedad + 10}

sumarVeinteAPropiedad :: Propiedades->Propiedades
sumarVeinteAPropiedad propiedad = propiedad {precio = precio propiedad + 20}

-- Pasar por el banco:

pasarPorElBanco :: Accion
pasarPorElBanco participante = participante {cantidadDeDinero = cantidadDeDinero participante + 40, tactica = "Comprador compulsivo"}

-- Gritar: 

gritar :: Accion
gritar participante = participante {nombreDeParticipante = "AAAH " ++ nombreDeParticipante participante}

-- Enojarse:

enojarse :: Accion
enojarse participante = participante {cantidadDeDinero = cantidadDeDinero participante + 50, acciones = gritar : acciones participante}

-- Subaste

type Participantes = Participante->Participante
type ParticipanteGanador = Participante

esOferenteSingular :: Participante->Bool
esOferenteSingular participante = tactica participante == "Oferente singular"

esAccionista :: Participante->Bool
esAccionista participante = tactica participante == "Accionista"

ganarSubasta :: Participante->Propiedades->Participante
ganarSubasta participante propiedad = participante {cantidadDeDinero = cantidadDeDinero participante - precio propiedad, propiedades = propiedad : propiedades participante }

tipoDeTactica :: Participante->Bool
tipoDeTactica = (\x -> esAccionista x || esOferenteSingular x)

subasta :: Participantes->Propiedades->ParticipanteGanador
subasta participante1 participante2 propiedad 
                                           | tipoDeTactica participante1 = ganarSubasta (participante1 propiedad)
                                           | tipoDeTactica participante2 = ganarSubasta (participante2 propiedad)

-- Pagar a accionsistas: 

restarCien :: Accion
restarCien participante = participante {cantidadDeDinero = cantidadDeDinero participante - 100}

sumarDoscientos :: Accion
sumarDoscientos participante = participante {cantidadDeDinero = cantidadDeDinero participante + 200}

pagarAAccionistas :: Accion
pagarAAccionistas participante
                             | esAccionista participante = sumarDoscientos participante
                             | otherwise = restarCien participante




