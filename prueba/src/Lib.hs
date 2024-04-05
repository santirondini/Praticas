triple :: Int->Int
triple x=3*x 

ciclon :: String->String 
ciclon x = x ++ " campeon"

promedio :: Float->Float->Float
promedio x y = (x + y) / 2

promedioDeNotas ::  Float->Float->Float->Float
promedioDeNotas nota1 nota2 nota3 = (nota1 + nota2 + nota3) / 3

esBisiesto :: Int->Bool 
esBisiesto valor = (mod valor 4 == 0 && mod valor 100 /= 0) || (mod valor 400 == 0)  

valorAbsoluto :: Float->Float
valorAbsoluto x
                | x < 0 = -x
                | otherwise = x

multiploDeTres ::   Int->Bool
multiploDeTres valor1 = mod valor1 3==0 

esCapicua :: String -> Bool
esCapicua n = reverse n == n

potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1           -- Cualquier número elevado a la potencia 0 es 1
potencia x n = x * potencia x (n - 1) -- Multiplica x por la potencia de x^(n-1)