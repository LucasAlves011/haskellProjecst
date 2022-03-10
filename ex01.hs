{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
soma::Int -> Int -> Int
soma x y = x + y 

retornar::Int -> Int
retornar 0 = 5
retornar 1 = 0
retornar 2 = 3


dobraLista:: [Int] -> [Int]
dobraLista [] = []
dobraLista (x:xs) = (2*x):dobraLista xs

--Compressão
dobraLista'::[Int] -> [Int]
dobraLista' l = [2*y | y <- l]

-- even retornar se o numero é par
dobraListaPar::[Int] -> [Int]
dobraListaPar l = [2*y | y <-l , odd  y]

--Somar elementos de uma tupla, e retorna uma lista de elementos
somarTupla::[(Int,Int)] -> [Int]
somarTupla l = [ x+y | (x,y) <- l]

{-Função de ALTA ORDEM é uma função que recebe como parâmetro uma função OU 
retorna uma função como resultado (ou as duas coisas )-}
aplica2Vezes :: (a -> a) -> a -> a
aplica2Vezes f x = f (f x)

