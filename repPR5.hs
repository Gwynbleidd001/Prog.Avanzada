--5 Dada una lista de enteros, retornar sus cuadrados, es decir, dado [x0, x1, . . . , xn]

cuad :: Int -> Int
cuad x = x*x

cuadList :: [Int] -> [Int]
cuadList [] = []
cuadList (x:xs) = map (cuad) (x:xs)

--6 CORREGIR
divi' :: Int -> [Int] -> Bool
divi' x (y:ys) | mod x y == 0 = True
               |otherwise = False

divi :: Int -> Bool
divi x = (divi' x [1..x] == True)
divi x = (divi' x [1..x] == False)

onlyDivs :: Int -> [Int]
onlyDivs 0 = [0]
onlyDivs 1 = [0,1]
onlyDivs x = filter divi  [1..x] 

--8 Dada una lista de naturales, retornar la suma de los cuadrados de la lista.

sumCuads :: [Int] -> Int
sumCuads xs = foldr (+) 0 (map (cuad) xs)

--9 Dada una lista de naturales, retornar la lista con sus sucesores.
plus1 :: Int -> Int
plus1 x = x+1

succs :: [Int] -> [Int]
succs xs = map (plus1) xs

--10 Dada una lista de enteros, sumar todos sus elementos.
totalInt :: [Int] -> Int
totalInt xs = foldl (+) 0 xs

--11 Definir el factorial usando fold.
facto :: Int -> Int
facto 0 = 1
facto 1 = 1
facto x = foldl (*) 1 [1..x]

--12 Redefinir la funcion and tal que and xs se verifica si todos los ele-
--mentos de xs son verdaderos. Por ejemplo: and [1<2, 2<3, 1/=0] = True,
--and [1<2, 2<3, 1 == 0] = False.
allTrue :: Bool -> Bool 
allTrue x | x == False = False
allTrue x | x == True = True

newAnd :: [Bool] -> Bool
newAnd xs | length(filter (allTrue) xs) == (length xs) = True 
          | otherwise = False

--13 Usando foldl o foldr definir una funcion tam::[a]->Int que devuelve la
--cantidad de elementos de una lista dada. Dar un ejemplo en los cuales foldr y
--foldl evaluen diferente con los mismos parametros. PENDIENTE

--PENDIENTEPENDIENTE


--tam :: [a] -> Int
--tam [] = 0
--tam (x:xs) = foldl () 0 (x:xs)

--Utilizando listas por comprension resolver:
--14 Dada una lista de enteros, retornar sus sucesores.

succsComp :: [Int] -> [Int]
succsComp xs = [x + 1 | x <- xs]

--15 Dada una lista de naturales, retornar sus cuadrados.
cuadss :: [Int] -> [Int]
cuadss xs = [x*x | x <- xs] 

--16 Dada una lista de enteros, retornar los elementos pares que sean mayores a 10.
paresMay10 :: [Int] -> [Int]
paresMay10 xs = [x | x <- xs, mod x 2 == 0 && x > 10]

--17 Dado un entero, retornar sus divisores.
divsN :: Int -> [Int]
divsN n = [x | x <- [1..n], mod n x == 0]

--18 Definir la funci ́on todosOcurrenEn :: Eq a => [a] -> [a] -> Bool 
--tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son ele-
--mentos de ys. Por ejemplo: todosOcurrenEn [1,5,2,5] [5,1,2,4] = True,
--todosOcurrenEn [1,5,2,5] [5,2,4] = False

todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
todosOcurrenEn xs ys = and [x `elem` ys | x <- xs]

--19 PENDIENTE HACER FUNCION PARA SACAR PRIMOS


--20 Dadas dos listas de naturales, retornar su producto cartesiano.
prodCart :: [Int] -> [Int] -> [(Int, Int)]
prodCart xs ys = [(x,y) | x <- xs, y <- ys]

--21 Dadas una lista y un elemento retornar el numero de ocurrencias del
--elemento x en la lista ys.
ocurr :: (Eq a) => [a] -> a -> Int
ocurr ys e = length [x | x <- ys, x == e]

--22. Escribir la funci ́on split2 :: [a] - > [([a],[a])], que dada una lista
--xs, devuelve la lista con todas las formas de partir xs en dos. Por ejemplo:
--split2 [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]),([1,2,3],[])].

split2 :: [a] -> [([a],[a])]
split2 xs = [(x,y) | n <- [0..length xs], x <- [take n xs], y <- [drop n xs]]

--23 Definir una funci ́on que, dada una lista de enteros, devuelva la suma de
--la suma de todos los segmentos iniciales.
--Por ejemplo: sumaSeg [1,2,3] = 0 + 1 + 3 + 6 = 10.

--sumaSeg :: [Int] -> [Int]
--sumaSeg xs = foldl (+) 0 [x | n <- [0..length xs], x <- take n xs]

--24 Definir la lista infinita de los n ́umeros pares.
infPares :: [Int]
infPares = [2*n | n <- [0..]]
