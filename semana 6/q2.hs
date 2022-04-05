import System.IO
import Data.List

type Pessoinha = (String,Char,Char)

manipula :: IO ()
manipula = do  putStrLn "Digite o seu nome: "
               nome <- getLine

               if nome == "" then do
                        quantidade <- getQuantidade
                        numeroCoabita <- getCoabita
                        covid  <- getCovid
                        print quantidade
                        appendFile "covid.txt" (show quantidade ++" pessoas responderam o questionario")
                        putStrLn (show quantidade ++" pessoas responderam o questionario")
                        putStrLn (show( calcularMediaCovid covid quantidade) ++" é a média de pessoas com confirmação de diagnóstico de infecção por COVID-19")
                        appendFile "covid.txt" (show( calcularMediaCovid covid quantidade) ++" é a média de pessoas com confirmação de diagnóstico de infecção por COVID-19")
                        putStrLn (show( calcularMediaCoabita numeroCoabita quantidade) ++" é a média de pessoas que coabitam com pessoas com diagnóstico ou suspeita de infecção por COVID-19.")
                        appendFile "covid.txt" (show( calcularMediaCoabita numeroCoabita quantidade) ++" é a média de pessoas que coabitam com pessoas com diagnóstico ou suspeita de infecção por COVID-19.")
               else do 
                       putStrLn "Voce teve covid : s/n"
                       confirmacao <- getLine
                       putStrLn "\nVoce convive com alguem que tenha covid: s/n"
                       coabitacao <- getLine
                       appendFile "covid.txt" (nome ++ " " ++ show confirmacao ++ " " ++ show coabitacao ++ "\n")
                       putStrLn "" -- só para pular uma linha no terminal
                       manipula



getQuantidade :: IO Int
getQuantidade = do
                arquivo <- readFile "covid.txt"
                return (length (lines arquivo))

calcularMediaCovid :: Int -> Int -> Int
calcularMediaCovid x y = x `quot` y

calcularMediaCoabita :: Int -> Int -> Int
calcularMediaCoabita x y = x `quot` y

getCovid  = do
          arquivo <- readFile "covid.txt"
          let segunda = filter (\d -> drop 1 d  == "s") (lines arquivo)
          return (length segunda)

getCoabita  = do
          arquivo <- readFile "covid.txt"
          let segunda = filter (\d -> drop 2 d  == "s") (lines arquivo)
          return (length segunda)

quebrarPalavra :: String -> [String]
quebrarPalavra s =  case dropWhile isEspaco  s of
                      "" -> []
                      s' -> w : quebrarPalavra s''
                            where (w, s'') = break isEspaco s'

quebrarLinha   :: String -> [String]
quebrarLinha s =  case dropWhile isFim  s of
                      "" -> []
                      s' -> w : quebrarLinha s''
                            where (w, s'') = break isFim s'

isFim ::Char -> Bool
isFim x | x == '\n' = True
        |  otherwise = False


isEspaco :: Char -> Bool
isEspaco x | x == ' ' = True
           | otherwise = False