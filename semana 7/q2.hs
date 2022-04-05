{-
Utilizando a linguagem funcional Haskell, defina uma função bag que recebe uma lista de 
elementos e retorna uma lista de pares, onde o primeiro elemento de cada par é um 
elemento da lista original e o segundo é o número de ocorrências deste elemento. 
Nesta segunda lista, cada elemento só ocorre uma vez. 
Por exemplo, bag [a,b,a,c,a,b] = [(a,3),(b,2),(c,1)]. 
-}

-- Exemplo de entrada bag [3,2,5,2,1,2,]     
--  bag["do","re","do","mi"]    
--  bag ['a','b','b','a','c']

--Função principal
bag :: Eq a => [a] ->  [(a, Int)]
bag x = limparDuplicados (calcula x)

--Remove Valores repetidos de listas
limparDuplicados :: Eq a => [a] -> [a]
limparDuplicados [] = []
limparDuplicados (x:xs) = x: limparDuplicados (remove x xs)

remove:: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) | x == y = remove x ys
                | otherwise = y:remove x ys

--Mostra a frequencia que aparece cada elemento
calcula :: Eq a => [a] -> [(a, Int)]
calcula lista = [(x, length(filter (==x) lista)) | x <- lista]


