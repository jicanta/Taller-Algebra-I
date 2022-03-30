identidad x = x
primero x y = x
segundo x y = y
constante5 x y z = 5

-- podemos definir una variable "a" que denota un tipo

g :: t -> t
g x = x
primero2 :: tx -> ty -> tx
primero2 x y = x
constante52 :: tx -> ty -> tz -> Int
constante52 x y z = 52

-- ":t" nos devuelve el tipo de una funcion

maximo x y | x >= y = x
           | otherwise = y

-- clases de tipos: conjuntos de tipos de datos a los que se les
-- puede aplicar un conjunto de funciones
-- Ej: Ord, Integral, Num, Eq
-- ":t expo" expo :: Num a => a -> a

expo x = x^10

square :: (Num p) => p -> p
square x = x*x

distintos :: (Eq p) => p -> p -> Bool
distintos x y = x /= y

cantidadDeSoluciones :: (Num t, Ord t) => t -> t -> Int
cantidadDeSoluciones b c | d > 0 = 2
                         | d < 0 = 0
                         | otherwise = 1
                         where d = b^2 - 4*c
                         
f1 x y z = x**y + z <= x + y**z

-- ":t f1" nos da f1 :: (Ord a, Floating a) => a -> a -> a -> Bool

f2 x y = (sqrt x) / (sqrt y)

-- ":t f2" nos da f2 :: Floating a => a -> a -> a

f3 x y = div (sqrt x) (sqrt y)

-- ":t f3" nos da f3 :: (Integral a, Floating a) => a -> a -> a

f4 x y z | x == y = z
         | x ** y == y = x
         | otherwise = y
         
         
-- ":t f4" nos da f4 :: (Eq p, Floating p) => p -> p -> p -> p    
-- como el output de las funciones solo puede tener un tipo,
-- haskell interpreta que x, y, z tienen el mismo tipo.
-- f4 5 5 True no funca     
         
f5 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z
         
-- ":t f5" nos da f5 :: (Eq a, Floating a) => a -> a -> p -> p
-- ahora f5 5 5 True si funca

-- tuplas
-- (True, (1, 2)) :: (Bool, (Int, Int))
-- (True. 1, 2) :: (Bool, Int, Int) 
-- fst nos devuelve el primer elemento de la tupla
-- snd nos devuelve el segundo elemento de la tupla
-- Ej: fst (1+4, 2) -> 5
-- snd (1, (2, 3)) -> (2, 3)

-- suma de vectores en r²

suma :: (Float, Float) -> (Float, Float) -> (Float, Float)
suma v w = ((fst v) + (fst w), (snd v) + (snd w))

-- o podemos usar pattern matching

suma2 (vx, vy) (wx, wy) = ((vx + wx), (vy + wy))

-- podemos usar pattern matching sobre constructores de tuplas
-- y numeros

esOrigen :: (Float, Float) -> Bool
esOrigen (0, 0) = True
esOrigen (_, _) = False

angulo0 :: (Float, Float) -> Bool
angulo0 (_, 0) = True
angulo0 (_, _) = False

{-
No podemos usar dos veces la misma variable

angulo45 :: (Float, Float) -> Bool
angulo45 (x, x) = True
angulo45 (_, _) = False
-}

angulo45 :: (Float, Float) -> Bool
angulo45 (x, y) = x == y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

normaVectorialSuma v1 v2 = normaVectorial (suma v1 v2)

-- EJERCICIOS --

estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | x <= 3 && y <= 3 = True
                      | 3 < x && x <= 7 && 3 < y && y <= 7 = True
                      | 7 < x && 7 < y = True
                      | otherwise = False
                      
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (vx, vy) (wx, wy) = vx * wx + vy * wy

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (vx, vy) (wx, wy) | vx < wx && vy < wy = True
                            | otherwise = False
                            invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como par ́ametro
(debe funcionar para elementos de cualquier tipo).
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (vx, vy) (wx, wy) = sqrt (dx * dx + dy * dy)
                                  where dx = vx - wx
                                        dy = vy - wy
                                        
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) | mod x 2 == 0 = 1
                         | mod y 2 == 0 = 2
                         | mod z 2 == 0 = 3
                         | otherwise = 4
                         
crearPar :: tx -> ty -> (tx, ty)
crearPar x y = (x, y)

invertir :: (tx, ty) -> (ty, tx)
invertir (x, y) = (y, x)
