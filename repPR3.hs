import System.Win32 (xBUTTON1)
--1
merg :: [Int] -> [Int] -> [Int]
merg [] ys = ys
merg xs [] = xs
merg (x:xs) (y:ys) | x >= y = y : merg (x:xs) (ys)
                   | y > x = x : merg  (xs) (y:ys) 

--2 PENDIENTE

--3
expo :: Int -> Int
expo 0 = 1
expo n = 2*(expo (n-1))

--4 REVISAR
binario :: Int -> [Int]
binario 0 = [0]
binario 1 = [1]
binario n = reverse ((mod n 2) : binario (div n 2))

--6
hamm :: (Eq a) => [a] -> [a] -> Int
hamm xs [] = 0
hamm [] ys = 0
hamm (x:xs) (y:ys) | x /= y = 1 + hamm (xs) (ys) 
                   |otherwise = hamm (xs) (ys) 

--7 PENDIENTE

--8
repN :: Int -> Int -> [Int]
repN z 0 = []
repN z n = z : repN z (n-1) 

--9
nElem :: [a] -> Int -> a
nElem (x:xs) 0 = x
nElem (x:xs) n = nElem (xs) (n-1)

--10 PENDIENTE
poscC :: (Eq a) => [a] -> a -> [Int]
poscC [] e = []
--CASO BASE FALTA
poscC (x:xs)  e | x == e = (length (x:xs) - length (xs)) : poscC (xs) e

--11
compact :: (Eq a) => [a] -> [a]
compact [] = []
compact [x] = [x]
compact (x:y:xs) | x == y = compact (y:xs) 
                 | x /= y = x : compact (y:xs) 