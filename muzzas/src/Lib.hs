{-

Se quiere saber cuántas pizzas completas de muzzarela hay que pedir, sabiendo la cantidad
de porciones que quiere cada persona (y suponiendo que cada pizza tiene 8 porciones). Lo 
que pide cada persona está representado por una tupla (sabor,cantidad).



type Sabor = String
type Cantidad = Int

type Pedido = (Sabor,Cantidad)  

cantidad = snd

--Un solo sabor posible : Muzzarela 

listaDePedidos :: [Pedido]
listaDePedidos = [("Muzzarella",3),("Muzzarella",7),("Muzzarella",2),("Muzzarella",5),("Muzzarella",4)]

-- ¿Por que no funcionó esto?
cuantasPizzasCompletasAPedir :: [Pedido]->Int
cuantasPizzasCompletasAPedir pedidos = foldl (+) 0 (cantidad)

--Solución

--sumarPorciones ::[Pedido]->Int
--sumarPorciones pedidos = sum.(map snd)
-}
-- TÉCNICA DE COMBATE

type CantidadDeHorasDeEntrenamiento = Int
type NombreDeUnObjetivo = String 
type PresionDelGolpe = Int
type Objetivo = String
type Poder = Int

poderDelGolpe :: CantidadDeHorasDeEntrenamiento->Poder
poderDelGolpe = (*15)

fortalezaDelObjetivo :: NombreDeUnObjetivo->Int
fortalezaDelObjetivo nombre = 2*length (nombre) 

presion :: NombreDeUnObjetivo->CantidadDeHorasDeEntrenamiento->PresionDelGolpe
presion nombre horas = div (poderDelGolpe horas) (fortalezaDelObjetivo nombre)

-- Presion = 15*cantidad de horas de entrenamineto / 2*cantidad de letras del objeto

-- HORA DE LECTURA

data Libro = UnLibro {
    autor :: String,
    nombre :: String,
    paginas :: Int
}

type Saga = [Libro]
type Biblioteca = [Libro]
type Lectura = [Libro]
-- Libros:

elVisitante = UnLibro "Stephen King" "El visitante" 592
shingeki = UnLibro "Hajime Isayama" "Shingeki no Kyojin | capitulos 1, 3 y 127" 120
fundacion = UnLibro "Isaac Asimov" "Fundación" 230
sandman = UnLibro "Neil Gaiman" "SandMan" 105
eragon = UnLibro "Christopher Paolini" "Eragon" 544
eldest= UnLibro "Christopher Paolini" "Eldest" 704
brisignr = UnLibro "Christopher Paolini" "Brisignr" 700
legado = UnLibro "Christopher Paolini" "Legado" 811

-- Sagas: 
sagaDeEragon :: [Libro]
sagaDeEragon = [eragon,eldest,brisignr,legado]

{-
Saber: 
1) Promedio de hojas
2) lectura Obligatoria si es de S.K, saga de eragon o Fundación 
3) biblioteca fantasiosa si algun libro de C.P o N.G
4) Nombre de bibliotevca = nomnbre de todos los titulos sin sus vocales
5) biblioteca ligera si todas sus lecturas tienen 40 paginas o menos

Consultar problema del foldl, por que no puedo hacerlo en el código pero si por ghci?
-}

-- 1) Promedio de hojas = Cantidad de hojas de todos los libros / cantidad de libros 

bibliotecaUTN :: [Libro]
bibliotecaUTN = [elVisitante,shingeki,fundacion,sandman,eldest,eragon,brisignr,legado]
listaDePaginas libro = map paginas libro

sumaDeHojasDeLosLibros :: [Libro]->Int
sumaDeHojasDeLosLibros biblioteca = foldl (+) 0 (listaDePaginas biblioteca)

cantidadDeLibros :: [Libro]->Int
cantidadDeLibros lista = length (lista)

promedioDeHojasDeLaBiblioteca :: [Libro]->Int 
promedioDeHojasDeLaBiblioteca lista = div (sumaDeHojasDeLosLibros lista) (cantidadDeLibros lista)

-- 2) Lectura obligatoria <=> SK, ERAGON o Fundación 

esDeStephenKing :: Libro->Bool
esDeStephenKing libro = autor libro == "Stephen King"

{-
Preguntar el uso del elem 

esDeLaSagaDeEragon :: Libro->Bool
esDeLaSagaDeEragon elLibro = elLibro `elem` sagaDeEragon

-}

esFundacion :: Libro->Bool
esFundacion libro = nombre libro == "Fundación"

lecturaObligatoria :: Lectura->Bool
lecturaObligatoria lectura = any (\x -> esFundacion x || esDeStephenKing x) lectura

-- 3) Biblioteca fantasiosa <=> algun libro de Cp o NG

esDeChristopherPaolini :: Libro->Bool
esDeChristopherPaolini libro = autor libro == "Christopher Paolini"

esDeNeilGaiman :: Libro->Bool 
esDeNeilGaiman libro = autor libro == "Neil Gaiman"

esFantasiosa :: Biblioteca->Bool
esFantasiosa biblio = any(\x -> esDeChristopherPaolini x || esDeNeilGaiman x) biblio

-- 4) Nombre de la biblioteca = Nombre de todos sus titulos sin las vocales
listaDeNombres biblioteca = map nombre biblioteca

esVocal :: Char->Bool
esVocal caracter = caracter == 'A' || caracter == 'I' || caracter == 'E' || caracter == 'O' || caracter == 'U' || caracter == 'a' || caracter == 'e' || caracter == 'i' || caracter == 'o' || caracter == 'u' 

filtrarVocalesEnUnaPalabra:: String->String 
filtrarVocalesEnUnaPalabra unNombre = filter(esVocal)unNombre
 
filtrarVocalesEnUnaListaDePalabras:: [String]->[String]
filtrarVocalesEnUnaListaDePalabras listaDePalabras = map filtrarVocalesEnUnaPalabra listaDePalabras 

filtrarVocalesEnNombresDeUnaBiblioteca:: Biblioteca->[String]
filtrarVocalesEnNombresDeUnaBiblioteca biblio = filtrarVocalesEnUnaListaDePalabras (listaDeNombres biblio)

nombreDeLaBiblioteca:: Biblioteca->String
nombreDeLaBiblioteca biblio = foldl (++) " " (filtrarVocalesEnNombresDeUnaBiblioteca biblio) 

-- 5) Biblioteca ligera <=> Todos sus libros tiene menos de 40 paginas

tieneMenosDeCuarentaPaginas :: Libro->Bool
tieneMenosDeCuarentaPaginas libro = paginas libro < 40

esLigeraLaBiblioteca :: Biblioteca->Bool
esLigeraLaBiblioteca biblio = all(tieneMenosDeCuarentaPaginas)biblio