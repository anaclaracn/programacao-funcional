-- 2. maiores-que: recebe um número e uma lista de números, retorna uma lista com os números
maiores_que :: Ord t => t -> [t] -> [t]
maiores_que _ [] = []
maiores_que n (c:r)
    | c > n     = c : maiores_que n r
    | otherwise = maiores_que n r


-- 8. gera_sequencia: recebe um número inteiro n positivo e retorna a lista [1,-1,2,-2,3,-3, ... ,n,-n]
gera_sequencia :: Int -> [Int]
gera_sequencia n
    | n <= 0    = [] 
    | otherwise = gera_sequencia (n-1) ++ [n, -n] 


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


-- 20. mediana: recebe uma lista de números e retorna a mediana deles
mediana :: (Ord t, Fractional t) => [t] -> t
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

