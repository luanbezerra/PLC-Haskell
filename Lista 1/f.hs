-- Calculadora

type Comando = String
type Valor = Int

faz :: [(Comando, Valor)] -> Int -> Int
faz [] atual = atual
faz (("Divide",0):xs) atual = -666
faz (("Multiplica",v):xs) atual = faz xs (atual*v)
faz (("Soma",v):xs) atual = faz xs (v+atual)
faz (("Subtrai",v):xs) atual = faz xs (atual - v)
faz (("Divide",v):xs) atual = faz xs (div atual v)

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa lista = faz lista 0

main = do
    a <- getLine
    let result = executa (read a)
    print result