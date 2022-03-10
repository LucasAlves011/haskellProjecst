{-
Crie uma função "organizaNomes" em Haskell que recebe uma lista de strings com nomes de pessoas  
(pode ser só o primeiro nome) e retorna uma lista de tuplas onde o primeiro elemento da tupla é um 
caractere e o segundo é uma lista com os nomes da lista inicial que começam com este caractere. 
Por exemplo:

-> organizaNomes ["jose", "lucas", "joao", "carlos", "antonio", "alfredo", "zoe"]

-> [('a', ["alfredo", "antonio"]), ('c', ["carlos"]), ('j', ["joao", "jose"]), ('l', ["lucas"]), ('z', ["zoe"])]

-}




{- 
    Como o enunciado não deixa claro, optei por fazer uma função caseSensitive, ou seja, ela vai 
    separar em tuplas diferentes nomes começados em 'a' e nomes começados em 'A'.
    Primeiro serão listadas todas as letras MAIÚSCULAS em ordem alfabética e depois todas as minúsculas em ordem alfabética.
    A ordem de saída das tuplas serão [('A',[]),('B',[]),...,('Z',[]),('a',[]),('b',[]),...,('z',[])] 
    OBS: A função não retornará tuplas com lista de nomes vazias.
-}


-- UTILIZE ESSA FUNÇÃO 
organizaNomes::[String] -> [(Char,[String])]
organizaNomes lista = filter filtraVazios (funcaoEntradaAuxiliar lista)

--Essa função é usada somente uma vez, e serve como porta de entrada para o algoritmo
funcaoEntradaAuxiliar:: [String] -> [(Char,[String])]
funcaoEntradaAuxiliar lista =  ('A',filter (\n -> head n == 'A') lista ):pegarListaNomes (succ 'A') lista


{-Funcao onde tudo acontece:
    1) Construir uma tupla com a letra da vez (começando em 'A' - MAIÚSCULO) e trazer todos os nomes começando com a letra da vez (utilizando filter e funcoes lambdas) mesmo que vazios. 
    2) A função chama a si mesma recursivamente para o sucessor da letra da vez ('A' -> 'B '-> 'C' -> ...) até a letra 'Z' - MAIÚSCULA. {linha 43}
    3) A funcão reinicia com um novo ponto de partida, dessa vez indo da letra 'a' - minúscula até o 'z' - minúsculo. {linha 42}
    4) Quando chegar no sucessor de 'z', que é o '{', a função retorna vazio e finaliza. {linha 41}
 -}                           
pegarListaNomes:: Char -> [String] -> [(Char,[String])]
pegarListaNomes '{' lista = []
pegarListaNomes '[' lista = ('a',filter (\n -> head n == 'a') lista ):pegarListaNomes (succ 'a') lista          -- Verifica de a - z, com a primeira letra minúscula
pegarListaNomes letra lista = (letra,filter (\n -> (head n) == letra) lista):pegarListaNomes (succ letra) lista -- Verifica de A - Z, com a primeira letra MAIÚSCULA


{- Aqui é a função que é usada no filter chamado em "organizaNomes"{linha 27}, ela é responsável por eliminar todas 
   as tuplas com listas de nomes vazias, ('a',[]) e ('F',[]) são exemplos de tuplas que serão excluídas-}
filtraVazios::(Char,[String]) -> Bool
filtraVazios (x,y) | null y = False
                   | otherwise  = True


-- Abaixo segue um array com nome de pessoas para ser usado em testes
nomes = ["Alice","Bianca","Carla","Carlos","Leonardo","Giovanni","Lucas","Rode","Pamela","Maria","Laura","Carol","Danila","Felipe","anita","marcelo","thiago","Paloma","paulo"]
