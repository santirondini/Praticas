
type NombreDelMaterial = String
type Cantidad = Int

data Material = UnMaterial {
    nombreMaterial :: NombreDelMaterial,
    cantidad :: Cantidad
} deriving (Eq,Show)

data Receta = UnaReceta {
    materiales :: [Material],
    tiempo :: Int
} deriving (Eq,Show)

data Personaje = UnPersonaje {
    nombre :: String,
    puntaje :: Int,
    inventario:: [Material]
} deriving (Eq,Show)


-- Materiales :
madera = UnMaterial "madera" 
fosforo = UnMaterial "fosforo" 
pollo = UnMaterial "pollo" 
sueter = UnMaterial "sueter" 
lana = UnMaterial "lana" 
agujas = UnMaterial "agujas" 
tintura = UnMaterial "tintura" 
fogata = UnMaterial "fogata" 

-- Recetas :
unafogata  = UnaReceta [madera 1, fosforo 1] 10
unpolloasado = UnaReceta [fogata 1,pollo 1] 300
unsueter  = UnaReceta [lana 1 ,agujas 1 ,tintura 1] 600

-- Steve : 
steve = UnPersonaje "Steve" 1500 [madera 3 ,fosforo 10 ,sueter 5]

-- Craftear 
-- Tomo como que los unicos elementos crafteables son una fogata, un pollo asado y un sueter:
-- Ver todo otra vez: hacer funciones que resten la cantidad de materiales. Si la cantidad es 1, se saca. Si no, se resta la cantidad: 

