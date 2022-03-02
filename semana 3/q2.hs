{-
    Um país deseja criar um protocolo de vacinação que informa a quantidade de meses para uma 
    pessoa tomar a vacina de acordo com 3 fatores:

    - idade.

    - presença de comorbidades

    - se é profissional de saúde.

    As regras são:

    a) caso seja profissional de saúde, o seu mês será 1. 

    b) pessoas acima de 70 anos também são vacinadas no mês 1. 

    c) abaixo de 70 anos, a cada cinco anos aumenta-se a quantidade de meses. 
       Assim, pessoas entre 65 e 70 (incompletos) são o mês 2, entre 60 e 65 são no mês 3, 
       e assim por diante. 

    d) caso a pessoa tenha comorbidade, seu mês de referência é reduzido em uma posição, 
       com o mínimo sendo o mês 1.

    Assim, crie uma função em Haskell que recebe uma tupla de três elementos (Int, Bool, Bool) representando 
    uma pessoa, onde o primeiro elemento é a idade, o segundo é um booleano informando se a pessoa tem 
    comorbidade e o terceiro é um booleano informando se a pessoa é profissional de saúde, e deve retornar a 
    quantidade de meses que a pessoa deverá esperar para ser vacinada.
-}

{- Explicação do código:
    A função que deve ser chamada é a funcaoPrincipal (idade,comorbidade,profissionalSaude)

    1) Ela primeiro verifica se a pessoa passada tem idade maior que zero, caso contrario, lança um erro e finaliza.

    2) Verifica se a pessoa passada tem 70 ou mais anos ou se ela é profissional de saúde, em ambos os 
      casos a função retorno 1 mês.

    3) Verifica se a pessoa tem comorbidade, chama a função de cálculo de meses e por final retira um mês
       como manda a regra de comorbidade.

    4) Por fim calcula a quantidade de meses para pessoas "comuns".

    Exemplos de entradas:
    funcaoPrincipal (32,False,False)
    funcaoPrincipal (0,False,False)
    funcaoPrincipal (70,True,False)
    funcaoPrincipal (47,False,False)
-}

funcaoPrincipal:: (Int,Bool,Bool) -> String 
funcaoPrincipal (idade,comorbidade,prof) | idade < 0 = error "Idade nao pode ser um numero negativo"
                                         | idade >= 70 || prof = "1 mes." 
                                         | comorbidade = show ((calcularMeses idade) -1) ++ " meses."
                                         | otherwise = show (calcularMeses idade) ++ " meses."

calcularMeses::Int -> Int
calcularMeses idade | idade <= 69 && idade >= 65 = 2
                    | idade <= 64 && idade >= 60 = 3
                    | idade <= 59 && idade >= 55 = 4
                    | idade <= 54 && idade >= 50 = 5
                    | idade <= 49 && idade >= 45 = 6
                    | idade <= 44 && idade >= 40 = 7
                    | idade <= 39 && idade >= 35 = 8
                    | idade <= 34 && idade >= 30 = 9
                    | idade <= 29 && idade >= 25 = 10
                    | idade <= 24 && idade >= 20 = 11
                    | idade <= 19 && idade >= 15 = 12
                    | idade <= 14 && idade >= 10 = 13
                    | idade <= 9  && idade >=  5 = 14
                    | idade <= 4  && idade >=  0 = 15