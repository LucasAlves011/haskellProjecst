{-
Batman precisa obter as áreas de Gotham City com maior índice de criminalidade e pediu para Robin criar um 
programa em Haskell para lhe ajudar. Robin tem acesso a todos os incidentes da cidade e para tanto pensou em um 
tipo de dados algébrico INCIDENTE. Este tipo getParametro ser um Roubo, Homicídio ou Agressão. O primeiro tem a informação 
do valor roubado (ponto flutuante), e os últimos dois possuem a quantidade de pessoas envolvidas no incidente. 
Cada Incidente tem um índice de gravidade calculado de acordo com seu parâmetro:

a. Roubo até $1000,50  – 5

b. Roubo acima de $1000,50  – 10

c. Homicidio = quantidade*20

d. Agressao = quantidade*5

Existem outros tipos de incidentes, mas eles não devem ser considerados na conta. Desta forma, crie um programa 
em Haskell que lê um arquivo contendo os incidentes de um bairro e deve retornar seu índice de criminalidade. 
OBS: A função readFile::String -> IO String lê um arquivo retorna o seu conteúdo.
-}

{-EXEMPLO DE COMO O ARQUIVO DEVE SER POPULADO PARA SER POSSÍVEL REALIZAR OS TESTES:


Roubo 132.43
Homicidio 3
Agressao 2
Roubo 565.36
Agressao 2
... (esses 3 pontos não devem ser inclusos no arquivo, estão somente para ilustrar a ideia de continuidade)

Cada linha do arquivo corresponde a um crime com seu respectivo valor/quantidade

-}

import System.IO ()

data Incidente  = Roubo Float | Homicidio Int | Agressao Int deriving (Show,Eq)



-- FUNÇÃO QUE DEVE SER CHAMADA PARA TESTE. ATENÇÃO !!! VOCÊ DEVE CRIAR UM "arquivo.txt" NA SUA MÁQUINA NO DIRETORIO ONDE ESSE PROGRAMA ESTÁ
--         E POPULA-LO DE MANEIRA SEMELHANTE AO EXEMPLO ACIMA:    
main :: IO ()              
main = do                  
    arq <- readFile "arquivo.txt" -- VOCÊ DEVE CRIAR UM ARQUIVO.TXT EM SUA MÁQUINA PARA TESTAR
    putStrLn ("O indice de violencia e de " ++ show (calcularIndice (montar (lines arq))))


-- Função utilizada para determinar qual parâmetro será utilizado para
getParametro :: Incidente  -> Int
getParametro (Roubo a) | a > 1000.50 =  10 
                       | otherwise =  5
getParametro (Homicidio a) =  a * 20
getParametro (Agressao a) =  a * 5

-- Função que faz o cálculo do indice
calcularIndice :: [Incidente] -> Int
calcularIndice = foldr ((+) . getParametro) 0

-- Função que lê a linha e instancia o tipo de dado correspondente
construir:: String -> [Incidente]
construir "" = []
construir palavra | cabeca == "Roubo" = [Roubo (read cauda)]
                  | cabeca == "Agressao" = [Agressao (read cauda)]
                  | cabeca == "Homicidio" = [Homicidio (read cauda)]
                  | otherwise = error "Arquivo com entradas inválidas, verifique os arquivo"
                where
                    cabeca =  head (words palavra)
                    cauda = b (tail (words palavra))

--Função que retira o elemento de uma lista unitária: Ex: [13] -> 13 ou ["nome"] -> "nome"
b:: [a] -> a
b [a] = a

-- Função que vai iniciar a transformação de Strings para uma lista do tipo Incidente
montar :: [String] -> [Incidente]
montar [] = []
montar (x:xs) | x == "" = montar xs
                | otherwise = construir x ++ montar xs
