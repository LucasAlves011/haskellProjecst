{-
Isso é um comentário 
	em 
		BLOCO
-}

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x> 100 then x else x*2 +1

myNameIs = "Meu nome é lucas"

resposta = 42

primeiro x | x == 'A' = 55 |  x == 'B' = -55 | otherwise = 0

funcaoPode :: Char -> Int
funcaoPode p = primeiro p

fib :: Int -> Int
fib 0 = 0
fib 1 = 1 
fib n = fib(n-1) + fib(n-2)

sim:: Bool
sim = True

maiorQue :: Bool
maiorQue = resposta > 52

quadrado:: Int -> Int
quadrado x = x * x

tudoIgual:: Int -> Int ->Int -> Bool
tudoIgual x y z = (x == y) && (y == z) 

-- Essa forma é esquisita mesmo 
maxi:: Int ->Int -> Int
maxi x y = if x>= y then x else if x == y then 0 else y

-- Forma interessante de se fazer um if (comandos guardados)
maxi':: Int ->Int -> Int
maxi' x y | x >=y = x
          | x == y = 0  -- É possível ter vários if's
          | otherwise = y -- Esse otherwise se comporta como else


vendas:: Int -> Int
vendas 0 = 10
vendas 1 = 5
vendas 2 = 15
vendas 3 = 27

-- Delegando 
quantidadeVendas':: Int -> Int
quantidadeVendas' 0 = vendas 0
quantidadeVendas' n = vendas n

-- Iterando para baixo
quantidadeVendas:: Int -> Int
quantidadeVendas 0 = vendas 0 -- Limite de casamento de padrão inferior
quantidadeVendas n = vendas n + quantidadeVendas(n - 1) -- Iterando para baixo

-- Iterando para cima
vendasCima:: Int -> Int
vendasCima 0 = vendas 0 -- Limite de casamento de padrão inferior
vendasCima 3 = vendas 3 -- Limite de casamento de padrão superior
vendasCima n = vendas n + vendasCima(n + 1) -- Iterando para cima

fat:: Int -> Int
fat 0 = 1 
fat n = n * fat(n-1) 

all4Equal:: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b) && (b == c) && (c == d)

pri :: (Char,Char,Char) -> Int
pri (a,b,c) | a == 'A' = 55 
            | a == 'B' = -55

sec :: Char -> Int
sec 'A' = 55
sec 'B' = -55   

-- teste teste
-- teste teste













