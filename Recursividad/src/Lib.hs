
-- Recursividad: 

-- length:

cantidad :: [a]->Int
cantidad [] = 0 -- Caso base 
cantidad (_:xs) = 1 + cantidad xs

-- take: Ordenar por prioridad  

toma :: Int->[a]->[a]
toma 0 _ = []
toma _ [] = []
toma n (x:xs) = x : toma (n-1) xs

-- Potencia :

elevar :: Int->Int->Int
elevar _ 0 = 1
elevar numero potencia = numero * elevar numero (potencia - 1)

-- factorial:

factorial 0 = 1 -- Caso base 
factorial n = n*factorial (n-1)

-- sumatoria:

sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
 

