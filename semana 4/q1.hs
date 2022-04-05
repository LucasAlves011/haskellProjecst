{-# LANGUAGE OverloadedStrings #-}

import Data.Text

type Json = [(String,[String])]

buscaJSON::String -> String -> [a]
buscaJSON conteudo path | path == '/' = conteudo
                         

-- putaria::[Char] -> [Text]
-- putaria x = splitOn "," (pack x)

-- x::String -> [String]
-- x [] = []
-- x (a:as) = 

-- "{"nome":"John","idade":30,"carros": { "carro1":"Ford","carro2":"BMW","carro3":"Fiat"}}"