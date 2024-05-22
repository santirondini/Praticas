import Data.List (delete)


type Tipo = String
type Nombre = String

data Pokemon = UnPokemon {
    nombre :: Nombre,
    tipo :: Tipo
} deriving (Eq,Show)

-- planta le gana a agua, agua a fuego y fuego a planta 

charmander = UnPokemon "Charmander" "fuego"
odish = UnPokemon "Odish" "planta"
bulbasaur= UnPokemon "Bulbasaur" "planta"
squirtle = UnPokemon "Squirtle" "agua"

jerarquia :: Tipo->Tipo->Tipo
jerarquia "fuego" "agua" = "agua"
jerarquia "agua" "fuego" = "agua"
jerarquia "planta" "fuego" = "fuego"
jerarquia "fuego" "planta" = "fuego"
jerarquia "agua" "planta" = "planta"
jerarquia "planta" "agua" = "planta"
jerarquia "agua" "agua" = "agua"
jerarquia "fuego" "fuego" = "fuego"
jerarquia "planta" "planta" = "planta"

lista = [charmander, bulbasaur , odish, squirtle]

aQuePokemonesLeGana :: Pokemon->[Pokemon]->[Pokemon]
aQuePokemonesLeGana pokemon pokemones = filter (\p -> jerarquia (tipo p) (tipo pokemon) == tipo pokemon) (delete pokemon pokemones)

pokemonMasPicanteEntreDos :: [Pokemon]->Pokemon->Pokemon->Pokemon
pokemonMasPicanteEntreDos pokemones poke1 poke2
                                    | length (aQuePokemonesLeGana poke1 pokemones) > length (aQuePokemonesLeGana poke2 pokemones) = poke1
                                    | otherwise = poke2

menosDos :: [Pokemon]->Pokemon->Pokemon->[Pokemon]
menosDos lista p1 p2 = delete p2 (delete p1 lista)

pokemonMasPicante :: [Pokemon]->Pokemon
pokemonMasPicante pokemones = foldl1 (\poke1 poke2 -> pokemonMasPicanteEntreDos pokemones poke1 poke2) pokemones

data Destino = UnGimnasio {nombreG:: String, siguiente:: Destino}
                   | UnaLiga {contrincantes:: [Pokemon] } deriving (Eq,Show)
                   
estaAlHorno :: Pokemon->Destino->Bool
estaAlHorno pokemon destino
                           | null (aQuePokemonesLeGana pokemon (contrincantes destino)) = True -- no le gana a ninguno 
                           | otherwise = False 

gym1 = UnGimnasio "Makena" gym2
gym3 = UnGimnasio "Bayside" premier
premier = UnaLiga [charmander]
gym2 = UnGimnasio "Moscu" gym3

puedoViajar :: Destino->Destino->Bool
puedoViajar destino1 destino2 
                            | destino1 == (siguiente destino2) || destino2 == (siguiente destino1) = True
                            | destino1 /= (siguiente destino2) || destino2 /= (siguiente destino1) = False 
                            | contrincantes destino1 /= [] ||  contrincantes destino2 /= [] = False
                            | otherwise = False  

-- Nos se como especificar cuando un destino es gimnasio y cuando es liga. Si pregunto puedoViajar premier gym me pone:
-- *** Exception: No match in record selector siguiente

