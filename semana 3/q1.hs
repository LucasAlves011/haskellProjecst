{-
	No sistema eleitoral americano quando um candidato a presidência ganha a votação por maioria de um determinado
estado, os votos dos delegados daquele estado são contabilizados para aquele candidato. Ganha a eleição quem
possuir mais votos de delegados, o que não necessariamente representa a maioria dos votos dos eleitores no
país. Crie uma função em Haskell para indicar o vencedor de uma eleição americana. Vamos simplificar
utilizando apenas dez estados, os quais são listados abaixo de acordo com seus números de delegados:
	    
1 - California = 55

2 - Texas = 38

3 - Florida = 29

4 - Nova York = 29

5 - Illinois = 20

6 - Pensilvânia = 20

7 - Ohio = 18

8 - Georgia = 16

9 - Michigan = 16

10 - Carolina do Norte = 15

Assumindo uma eleição com dois candidatos, A e B, crie uma função em Haskell que recebe uma tupla de 10 elementos
 indicando  em cada posição o candidato que venceu em cada estado na ordem listada acima. Por exemplo, se o candidato A
  aparece na posição 1 então ele venceu no estado da Califórnia, já se ele aparece na posição 8, ele ganhou o estado da
   Georgia. Desta forma, calcule a quantidade de votos de delegados que cada candidato terá no colégio eleitoral e
    RETORNE O VENCEDOR DA ELEIÇÃO, OU SEJA, AQUELE QUE CONSEGUIU O MAIOR NÚMERO DE VOTOS DE DELEGADOS.    
-}


{- Explicação do código :
 
 
 O código funciona da seguinte forma, o programa soma os delegados de maneira positiva 
 para aparições de 'A' na entrada e soma negativamente delegados na aparição de 'B' na entrada.
 Se o resultado der positivo o candidato A ganha e se der negativo o candidato B ganha. 

 exemplos de entradas:
 contarVotos ('A','B','A','B','A','A','A','B','B','B')
 contarVotos ('B','A','B','A','B','B','B','A','A','B')
 contarVotos ('A','B','A','B','A','A','A','B','A','B')
 contarVotos ('B','B','A','B','A','B','A','B','A','B')
 contarVotos ('B','B','A','B','A','A','A','B','B','A')
 contarVotos ('B','B','A','B','A','B','A','B','B','B')
-}

type Entrada = (Char,Char,Char,Char,Char,Char,Char,Char,Char,Char)

retorno :: Entrada -> Int
retorno (california,texas,florida,newYork,illinois,pensilvania,ohio,georgia,michigan,carolina) =
    pri california + seg texas + tec florida + tec newYork + qui illinois
    + qui pensilvania + set ohio + oit georgia + oit michigan + dez carolina

contarVotos :: Entrada -> String
contarVotos x | retorno x > 0 = "Candidato 'A' ganhou a eleicao." 
              | retorno x < 0 = "Candidato 'B' ganhou a eleicao."
              | otherwise = "A eleicao empatou."

pri:: Char -> Int
pri 'A' = 55
pri 'B' = -55

seg :: Char -> Int
seg 'A' = 38
seg 'B' = -38

tec :: Char -> Int
tec 'A' = 29
tec 'B' = -29

qui :: Char -> Int
qui 'A' = 20
qui 'B' = -20

set :: Char -> Int
set 'A' = 18
set 'B' = -18

oit :: Char -> Int
oit 'A' = 16
oit 'B' = -16

dez :: Char -> Int
dez 'A' = 15
dez 'B' = -15