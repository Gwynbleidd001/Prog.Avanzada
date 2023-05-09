--PRACTICO 8

--1 Definir la funciÓn nand a b = not (a && b) en Haskell sin utilizar not y &&.
nand :: Bool -> Bool -> Bool
nand True True = False
nand True False = True
nand False True = True
nand False False = True

--2 Definir en Haskell la función maj : : Bool −> Bool −> Bool −> Bool que retorna True sii al menos 2 argumentos son True.
maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj True False x = x
maj False True x = x
maj False False _ = False

--3 Escribir los siguientes cuantificadores:
-- 1ro (∃i : 0 ≤ i < #xs : p xs.i)

predA :: (a -> a -> Bool) -> [a] -> Bool
predA p xs = and [p (x) (y) | x <- xs, y <- xs]

--2do (∀i : 0 ≤ i < #xs : p xs.i)
predE :: (a -> a -> Bool) -> [a] -> Bool
predE p xs = or [p (x) (y) | x <- xs, y <- xs]

