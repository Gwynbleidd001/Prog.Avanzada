--1
inf1 :: Int -> [Int]
inf1 x | x == 1 = x : inf1 x
       | otherwise = []

--2 Averiguar como definir que recibe solo naturales, en este caso para evitar su uso se plantearan casos.
infNat :: Int -> [Int]
infNat x | x >= 0 = x : infNat (x+1) 
         | otherwise = []
         
--3
primNats :: Int -> Int -> [Int]
primNats x y | (x /= 0 && y >= 0) = y : primNats (x-1) (y+1)
             | x == 0 = []
             
primNat :: Int -> Int -> Bool
 
 
 
--4 PENDIENTE (HOGAR)

--5
cuad :: Int -> Int
cuad x = x*x

cuadList :: [Int] -> [Int]
cuadList [] = []
cuadList (x:xs) = map cuad (x:xs)

--6 PENDIENTE
--3
primNats :: Int -> Int -> Bool
primNats x y = mod x y == 0

--6
onlyDivs :: Int -> [Int] -> [Int]
onlyDivs x (y:ys)| mod x y == 0 = y : onlyDivs x ys
onlyDivs x (y:ys)| mod x y /= 0 = onlyDivs x ys

divs :: Int -> [Int]
divs 0 = [1,2..]
divs x = filter (onlyDivs x) (primNats x) 


--UTILIZAR fold (divs x) [1..n] = Dominio de los números que pueden ser divisores de x
divsInt

--7 FUNCIONA, ANALIZAR LA LISTA POR COMPRENSIÓN
onlyPrim :: Int -> Bool
onlyPrim 1 = False
onlyPrim n = and [(mod n i) /= 0 | i <- [2..(n-1)]]

primos :: [Int] -> [Int]
primos [] = []
primos xs = filter onlyPrim xs 

--8 CASA

--9 LISTO
sucs :: Int -> Int
sucs x = x + 1

lsSucs :: [Int] -> [Int]
lsSucs [] = []
lsSucs xs = map sucs xs 

--10 RECORDAR QUE CON CUALQUIER TIPO DE FOLD ES NECESARIO QUE LA FUNCIÓN SEA ASOCIATIVA Y CONMUTATIVA
sumElmts :: [Int] -> Int
sumElmts [] = 0
sumElmts xs = foldl (+) 0 xs

--11
fact :: Int -> Int
fact 0 = 1 
fact n = foldl (*) 1 [1..n] 

--12 RAZONAR (listo)
redefAnd :: [Bool] -> Bool
redefAnd [] = True
redefAnd (x:xs) | x == True = redefAnd xs
                |otherwise = False
                
--13 DUDA ¿DIFERENCIA ENTRE LA PRIMERA Y LA SEGUNDA?
sumListl :: Int -> a -> Int
sumListl n _ = n+1

taml :: [a] -> Int
taml xs = foldl (sumListl) 0 xs

--SEGUNDA FORMA
--contElem :: Int -> [a] -> Int
--contElem y [] = 0
--contElem y xs = y + 1

--tam :: [a] -> Int
--tam xs = foldl (contElem) 0 xs 

--14 IMPORTANTE: La operación que se lleva sobre la variable dependiente 
--se lleva a cabo antes de declarar de que depende.
sucss :: [Int] -> [Int]
sucss xs = [x + 1|x <- xs] 

--15 CONSULTAR SI EL RAZONAMIENTO ES CORRECTO
--IMPORTANTE: Si uno compara a xs con 0 no considera que xs es una lista y no un entero único
cuadNats :: [Int] -> [Int]
cuadNats xs = [x*x | xs >= [0], x <- xs]

--16 CONSIDERAR QUE LA LISTA ESTARA FORMADA POR LOS ELMTS PARES (mod x 2 == 0) Y MAYORES A 10 (x > 10)
--IMPORTANTE: La condición tiene que ser evaluada sobre x luego de tomar forma a partir de los elementos de xs.
parsMaysTen :: [Int] -> [Int]
parsMaysTen xs = [x | x <- xs, (x > 10) && (mod x 2 == 0)]

--17 IMPORTANTE: Luego del |, se establece el conjunto con el cual los X de la lista intermedia tomarán forma 
--y las condiciones que deben cumplir para ser consideradas en la lista final (resultado de la función)
soloDivs :: Int -> [Int]
soloDivs n = [x | x <- [1..n], mod n x == 0]

--18 PENDIENTE
todosOcurrenEn :: (Eq a) => [a] -> [a] -> Bool
todosOcurrenEn [] _ = False
todosOcurrenEn xs ys 

--19
primsEntreN :: Int -> [Int]
primsEntreN n = [x | x <- [2..n], mod x (n-1) /= 0 && mod x n == 0]


