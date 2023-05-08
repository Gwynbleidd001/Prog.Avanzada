--1
merge2 :: [Int] -> [Int] -> [Int]
merge2 [][] = []
merge2 xs [] = xs
merge2 [] ys = ys
merge2 (x:xs)(y:ys) | x >= y = y : x: merge2 xs ys
merge2 (x:xs)(y:ys) | y > x = x : y: merge2 xs ys

--2 Completo - ANALIZAR COMPORTAMIENTO
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x] --DUDA: ¿En qué parte se hace la recursión?
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted 

--3 Completo
expon :: Int -> Int
expon 1 = 2
expon x | x > 1 = 2 * expon (x-1)  

--4 Corregir
binario :: Integer -> [Integer]
binario 0 = 0 
binario 1 = 1
binario x = mod x 2 : binario (div x 2)

--5 Pendiente

--6
hamm :: (Eq a) => [a] -> [a] -> Int
hamm [] []= 0
hamm (x:xs) [] = 0
hamm [] (y:ys) = 0
hamm (x:xs) (y:ys)| x == y = hamm xs ys
hamm (x:xs) (y:ys)| x /= y = 1 + hamm xs ys

--7 Pendiente

--8
repN :: Int -> Int -> [Int]
repN z 0 = []
repN z n = z : repN z (n-1) 



