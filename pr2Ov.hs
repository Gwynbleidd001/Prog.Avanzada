
hd :: [a] -> a
hd (x:xs) = x 

tl :: [a] -> [a]
tl (x:xs) = xs

lst :: [a] -> a
lst (x:xs) = hd(reverse(x:xs))

inn :: [a]-> [a]
inn [x] = []
inn (x:xs) = x : inn xs

maxTres :: Int -> Int -> Int -> Int
maxTres x y z = max x (max y z)

conct :: [a]->[a]->[a]
conct [][] = []
conct []ys = ys
conct (x:xs) ys = x : conct xs ys

tom :: Int -> [a] -> [a]
tom n [] = []
tom 0 (x:xs) = []
tom n (x:xs) = x : tom (n-1) xs

tirar :: Int -> [a] -> [a]
tirar n [] = []
tirar 0 (x:xs) = []
tirar n (x:xs) = tirar (n-1) xs

--Forma correcta abs
abs :: Int -> Int
abs x | x < 0 = -x
      |otherwise = x

--Otra forma correcta de abs
abs :: Int -> Int
abs x |(x >= 0) = x
      |(x< 0) = (x*(-1))

--Forma incorrecta abs
--abs :: Int -> Int
--abs x | (x >= 0) = x
--abs x | x < 0 = -(x)

edad :: (Int,Int,Int) -> (Int,Int,Int) -> Int
edad (d1,m1,a1)(d2,m2,a2)|(a1 == a2) = 0 --Transcurren en el mismo año
                         |(a1 /= a2) && (m1 > m2) = 0 --Pasan en dists. años pero no ha pasado un año
                         |(a1 /= a2) && (m1 = m2) && (d1 <= d2) = (-1)*(a1 - a2)
                         |(a1 /= a2) && (m1 < m2) = (-1)*(a1 - a2)


--7
xor :: Bool -> (Bool -> Bool)
xor True _ = True
xor False x = x

--8 Funciona
prim :: Int -> Int -> Bool
prim x 1 = False --Condición que permitirá saber si X es primo al ser negada en prim'.
prim x y = mod x y == 0 || prim x (y-1) --Condición que permitirá saber si X es no primo o llegar al caso base para asegurar que X es primo.

prim' :: Int -> Bool
prim' 1 = False
prim' 2 = True
prim' x = not (prim x (x-1)) 

--9 Listo, pero si es primo el núm. inicial lo incluye en la lista, falta corregir eso.
natPrim :: Int -> [Int]
natPrim 1 = []
natPrim x | (prim' x == False) = natPrim(x-1)
natPrim x | (prim' x == True) = x : natPrim(x-1)

--10 
revList :: [a] -> [a]
revList [] = []
revList (x:xs) = (revList(xs)) ++ [x]

--11 CONTROLAR
listsIgua :: [a]->[a]->Bool
compLists [][] = False
compLists xs[] = False
compLists []ys = False
compLists (x:xs) (y:ys) | (x == y) = compLists xs ys
compLists (x:xs) (y:ys) | (x /= y) = False

--12 PENDIENTE
palin :: [Char] -> [Char] -> Bool
palin [] [] = False
palin x [] = False
palin [] y = False
palin (x:xs) (y:ys) | x == y = 
palin (x:xs) (y:ys) = (palin (x:xs) (reverse (y:ys)))

--13
raiRs :: Int -> Int -> Int -> Int
raiRs a b c | (sqrt ((b*b) - 4*a*c)) > 0 = 2
            | (sqrt ((b*b) - 4*a*c)) == 0 = 1
            | (sqrt ((b*b) - 4*a*c)) < 0 = 2