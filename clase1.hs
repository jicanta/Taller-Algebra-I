f x y = x * x + y * y

g x y z = x + y + z * z

doble x = 2 * x

suma x y = x + y

normaVectorial x1 x2 = sqrt (x1 * x1 + x2 * x2)

funcionConstante8 x = 8

-- podemos usar guardas para definir funciones por casos
-- el orden de las guardas cambia el comportamiento de la funcion
-- haskell ejecuta las guardas de arriba para abajo

ff n | n == 0 = 1
     | otherwise = 0
    
gg n | n == 0 = 1
     | n == 1 = 0
    
signo n | n > 0 = 1
        | n < 0 = -1
        | otherwise = 0
        
maximo x y | x >= y = x
           | otherwise = y

f1 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8

-- otra posibilidad usando pattern matching
-- gg1 es equivalente a gg

gg1 0 = 1
gg1 1 = 0

-- signo con pattern matching

signo2 0 = 0
signo2 n | n > 0 = 1
         | otherwise = -1
         
-- cantidadDeSoluciones(b, c) devuelve la cantidad de soluciones de
-- x² + bx + c = 0
-- "where" sirve para no repetir codigo

cantidadDeSoluciones b c | d > 0 = 2
                         | d == 0 = 1
                         | otherwise = 0
                         where d = b^2 - 4*c
                         
-- en haskell podemos explicitar el tipo de datos del
-- dominio y codominio de las funciones que definimos.
-- a esto lo llamamos dar la signatura de la funcion

suma2 :: Int -> Int -> Int
suma2 a b = a + b

-- funcionRara(a, b, c) devuelve True si C es True.
-- sino devuelve True si x >= y

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara _ _ True = True
funcionRara x y False = x >= y

absoluto :: Int -> Int
absoluto x | x > 0 = x
           | otherwise = (-1) * x

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | absX > absY = absX
                   | otherwise = absY
                   where absX = absoluto(x)
                         absY = absoluto(y)    
                         
maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c = maximo (maximo a b) c
   
algunoEs0PM :: Float -> Float -> Bool
algunoEs0PM _ 0 = True
algunoEs0PM 0 _ = True
algunoEs0PM _ _ = False

algunoEs0G :: Float -> Float -> Bool
algunoEs0G a b | a == 0 || b == 0 = True
               | otherwise = False
               
ambosSon0PM :: Float -> Float -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM _ _ = False

ambosSon0G :: Float -> Float -> Bool
ambosSon0G a b | a == 0 && b == 0 = True
               | otherwise = False
               
esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b | mod a b == 0 = True
                 | otherwise = False
                 
digitoUnidades :: Int -> Int
digitoUnidades a = mod a 10

digitoDecenas :: Int -> Int
digitoDecenas a = mod a 100
               
