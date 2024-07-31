-- Ana Clara Carvalho Nascimento   -   202310179
-- Isadora Gomes Melo Cunha              -   202310543
-- Grupo 2

-- 2. maiores-que: recebe um número e uma lista de números, retorna uma lista com os números
maiores_que :: Ord t => t -> [t] -> [t]
maiores_que _ [] = []
maiores_que n (c:r)
    | c > n     = c : maiores_que n r
    | otherwise = maiores_que n r


-- 5. remover_ultimo: recebe uma lista e retorna a lista sem o último elemento 
remover_ultimo ::[t] -> [t]
remover_ultimo [] = [] 
remover_ultimo [_] = [] 
remover_ultimo (c:r) = c : remover_ultimo r 


-- 8. gera_sequencia: recebe um número inteiro n positivo e retorna a lista [1,-1,2,-2,3,-3, ... ,n,-n]
gera_sequencia :: Int -> [Int]
gera_sequencia n
    | n <= 0    = [] 
    | otherwise = gera_sequencia (n-1) ++ [n, -n] 


-- 11. somatorio: recebe uma lista de números e retorna a soma deles
somatorio :: (Real t) => [t] -> t
somatorio [] = 0
somatorio (c:r) = c + somatorio r

-- 14. interseccao: recebe duas listas sem elementos repetidos e retorna uma lista com os elementos que são comuns às duas
interseccao :: Eq t => [t] -> [t] -> [t]
interseccao [] _ = []
interseccao (c:r) lista
    | procuraElemento c lista = c : interseccao r lista
    | otherwise                = interseccao r lista
    where
        procuraElemento :: Eq t => t -> [t] -> Bool
        procuraElemento _ [] = False
        procuraElemento x (c:r)
            | x == c    = True
            | otherwise = procuraElemento x r


-- 17. insere_ordenado: recebe uma lista de números em ordem crescente e um número qualquer, retorna uma lista de números em ordem crescente com os elementos da lista inicial mais o número passado. 
insere_ordenado :: (Real t) => [t] -> t -> [t]
insere_ordenado [] n = [n]
insere_ordenado (c:r) n
    | n <= c    = n : c : r
    | otherwise = c : insere_ordenado r n

-- 20. mediana: recebe uma lista de números e retorna a mediana deles
mediana :: (Ord t, RealFrac t) => [t] -> t
mediana lista
    | impar n   = listaOrdenada !! indiceCentral
    | otherwise = (listaOrdenada !! (indiceCentral - 1) + listaOrdenada !! indiceCentral) / 2

    where
        listaOrdenada = ordena lista
        n = contaElementos lista
        indiceCentral = n `div` 2

        contaElementos :: [t] -> Int
        contaElementos [] = 0
        contaElementos (c:r) = 1 + contaElementos r

        impar :: Int -> Bool
        impar x = x `mod` 2 /= 0

        ordena :: Ord t => [t] -> [t]
        ordena [] = []
        ordena (c:r) = ordena [x | x <- r, x <= c]
                        ++ [c]
                        ++ ordena [x | x <- r, x > c]


-- 23. rodar_direita: recebe um número natural, uma lista e retorna uma nova lista onde a posição dos elementos mudou como se eles tivessem sido "rodados" 
rodar_direita :: Int -> [t] -> [t]
rodar_direita _ [] = []
rodar_direita 0 lista = lista
rodar_direita n lista = rodar_direita (n-1) (ultimoElemento lista : remover_ultimo lista)
    where
        ultimoElemento :: [t] -> t
        ultimoElemento [x] = x
        ultimoElemento (_:r) = ultimoElemento r


-- 26. media: Recebe uma lista de números e retorna a média aritmética deles
media :: (Real t, RealFrac t) => [t] -> t
media lista = somatorio lista / fromIntegral (contaElementos lista)
    where
        contaElementos :: [t] -> Int
        contaElementos [] = 0
        contaElementos (c:r) = 1 + contaElementos r


-- 29. seleciona: recebe uma lista qualquer e uma lista de posições, retorna uma lista com os elementos da primeira que estavam nas posições indicadas 




-- 32. primo: verifica se um número é primo ou não 
primo :: Int -> Bool
primo n
    | n <= 1    = False
    | otherwise = not (dividePor n 2)
  where
    dividePor :: Int -> Int -> Bool
    dividePor n divisor
        | divisor * divisor > n = False
        | n `mod` divisor == 0  = True
        | otherwise               = dividePor n (divisor + 1)


-- 33. soma_digitos: recebe um número natural e retorna a soma de seus dígitos 
soma_digitos :: Int -> Int
soma_digitos n
    | n < 10    = n
    | otherwise = n `mod` 10 + soma_digitos (n / 10)


-- 35. compactar: recebe uma lista de números e transforma todas as repetições em sub-listas de dois elementos: sendo o primeiro elemento o número de repetições encontradas e o segundo elemento é o número que repete na lista original. Os números que não repetem na lista original não devem ser alterados. 