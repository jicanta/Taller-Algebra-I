sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria x = head x + sumatoria (tail x)

longitud :: [Integer] -> Integer
longitud [] = 0
longitud x = 1 + longitud (tail x)

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n x | head x == n = True
              | otherwise = pertenece n (tail x)
              
-- alternativa:
pertenece2 :: Int -> [Int] -> Bool
pertenece2 _ [] = False
pertenece2 x l = (x == head l) || pertenece x (tail l)

primerMultiploDe45345 :: [Int] -> Int
primerMultiploDe45345 [] = -1
primerMultiploDe45345 x | mod (head x) 45345 == 0 = head x
                        | otherwise = primerMultiploDe45345 (tail x) 

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

sumarN :: Int -> [Int] -> [Int]
sumarN _ [] = []
sumarN n (x:xs) = (x+n):(sumarN n xs)

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x:(pares xs)
             | otherwise = pares xs
             
quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar n (x:xs) | x == n = xs
                | otherwise = x:(quitar n xs)
                
hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs
                    
eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = eliminarRepetidos xs
                         | otherwise = x:(eliminarRepetidos xs)
                         
maximo :: [Int] -> Int
maximo [] = -100000000
maximo (x:xs) | x > maxi = x
              | otherwise = maxi
              where maxi = maximo xs
              
minimo :: [Int] -> Int
minimo [] = 1000000
minimo (x:xs) | x < mini = x
              | otherwise = mini
              where mini = minimo xs
              
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar x = (mini):(ordenar (quitar mini x))
            where mini = minimo x

agregarAlFinal :: Int -> [Int] -> [Int]
agregarAlFinal n [] = [n]
agregarAlFinal n (x:xs) = x:(agregarAlFinal n xs)
            
reverso :: [Int] -> [Int]
reverso [] = []
-- operador ++ concatena : reverse (x:xs) = (reverso xs) ++ [x]
reverso (x:xs) = agregarAlFinal x (reverso xs) 


