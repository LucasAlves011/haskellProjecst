{-Blockchain é basicamente um banco de dados distribuído onde os dados são armazenados em blocos onde cada bloco possui transações financeiras e uma referência para o próximo bloco formando uma corrente. Assumindo os tipos de dados abaixo e que seu programa em possui uma versão inicial da cadeia de blocos, crie um programa em Haskell que lê uma transação via entrada padrão (conta origem, conta destino, valor, indíce do bloco) e em seguida navega na cadeia de blocos até achar o bloco com o índice informado e adiciona a transação a lista de transações do referido bloco. 

data Transacao = Transacao { de :: String -- Conta que paga

                                                , para :: String -- Conta que recebe

                                                 , valor :: Float -- Quanto está pagando

                                                } deriving (Show)

data Bloco = Bloco { indice :: Int -- Indice do bloco

                                 , trs :: [Transacao] -- Lista de transações de um bloco

                                 , proximo :: Maybe Bloco -- Proximo bloco

                                 } deriving (Show)

OBS: para definir um valor do tipo bloco é só especificar (Bloco ind trs prox), onde ind é um inteiro, trs é uma lista de transações e prox é um Maybe Bloco. O mesmo se aplica ao tipo Transacao.
-}

data Transacao = Transacao { de :: String , para :: String , valor :: Float } deriving (Show)

data Bloco = Bloco { indice :: Int , trs :: [Transacao] , proximo :: Maybe Bloco } deriving (Show)

-- Cadeia de Blocos iniciais

-- Transações do bloco 1
t1 = Transacao "lucas" "matheus" 3
t2 = Transacao "matheus" "ricardo" 25

b1 = Bloco 1 [t1,t2] (Just b2)

-- Transações do bloco 2
t4 = Transacao "matheus" "lucas" 95
t5 = Transacao "carlos" "lucas" 12
b2 = Bloco 2 [t4,t5] (Just b3)

--Transações do bloco 3

t7 = Transacao "carlos" "laura" 70
t8 = Transacao "monique" "blenda" 62
t9 = Transacao "antenor" "giovana" 57
b3 = Bloco 3 [t7,t8,t9] Nothing

-- receber :: Transacao -> Int -> String
-- receber (Transacao de para prox) indece =

getTransacoes :: Maybe Bloco -> Transacao -> [Transacao]
-- getTransacoes (Bloco _ y Nothing) = y
getTransacoes (Just(Bloco _ y _)) x  = x : y
-- getTransacoes (Bloco x y (Just z)) = y ++ getTransacoes z

getBloco :: Int -> Maybe Bloco -> Transacao -> Maybe Bloco
getBloco _ Nothing  _ = Nothing
getBloco index (Just (Bloco indice y prox)) tr |  indice == index = Just (Bloco indice (tr:y) prox)
                                               |  otherwise = getBloco index prox tr

func :: Int -> Transacao -> [Transacao]
func indice tr = getTransacoes (getBloco indice (Just b1) tr) tr

t10 = Transacao "teste" "teste" 99