{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
data Transacao = Transacao { de :: String -- Conta que paga
 , para :: String -- Conta que recebe
, valor :: Float -- Quanto está pagando
 } deriving (Show)

data Bloco = Bloco { indice :: Int -- Indice do bloco
, trs :: [Transacao] -- Lista de transações de um bloco
, proximo :: Maybe Bloco -- Proximo bloco
} deriving (Show)

b0 :: Bloco -- Bloco inicial
b0 = Bloco 0 t0 (Just b1)
t0 :: [Transacao]
t0 = [Transacao "P1" "P2" 10.0, Transacao "P1" "P2" 10.0, Transacao "P1" "P2" 10.0]

b1 :: Bloco
b1 = Bloco 1 t1 (Just b2)
t1 :: [Transacao]
t1 = [Transacao "P1" "P2" 2.0, Transacao "P1" "P2" 2.0, Transacao "P1" "P2" 2.0]

b2 :: Bloco
b2 = Bloco 2 t2 (Just b3)
t2 :: [Transacao]
t2 = [Transacao "P1" "P2" 5.0, Transacao "P1" "P2" 5.0, Transacao "P1" "P2" 5.0]

b3 :: Bloco
b3 = Bloco 3 t3 Nothing
t3 :: [Transacao]
t3 = [Transacao "P1" "P2" 1.0, Transacao "P1" "P2" 1.0, Transacao "P1" "P2" 1.0]

somaValorDasTransacoes :: [(Char,Int)] -> [(Char,Int)]
somaValorDasTransacoes = (filter (\ (a,b) -> a == a))

-- ds::[Transacao] -> [(String,Int)]
-- ds = map (\n -> (de,valor))

-- func1::Bloco -> [String]
-- func1 trs [] = []
-- func1 
teste :: [(Char, Int)]
teste = [('a',32),('a',53),('b',35),('a',10),('b',76),('c',1)]