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

--Se utiliza True como 1er parámetro del pred porque funciona como --neutro del PARA TODO.
evenGen :: Int -> [a] -> Bool
evenGen n xs = even xs!!n

--REVISAR COMO FUNCIONA el odd de Haskell
oddGen :: Int -> [a] -> Bool
oddGen n xs = odd xs!!n

paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo is xs p = foldl (&&) True xs
                   where ys = [p i xs | i <- xs]

--CORREGIR 2do (∀i : 0 ≤ i < #xs : p xs.i)
existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe ind xs p = and [p xs i | i <- ind]

--4 Utilizando las ideas asociadas a listas por comprensión, y las --funciones sum, product, y length, escribir los cuantificadores --de sumatoria, productoria y contatoria para ejemplos concretos.

sumatoria :: [Int] -> Int
sumatoria xs = sum [x | x <- xs]

productoria :: [Int] -> Int

contatoria :: [Int] -> Int
contatoria xs = sum [x | x <- (length xs) - 1]


