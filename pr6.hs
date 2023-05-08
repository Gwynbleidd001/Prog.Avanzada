--Práctico 6: Programación Funcional: Tipos de datos recursivos

--Ejerc. 1) Definir el tipo Nat.

data Nat = Zero | Succ Nat deriving (Show, Eq)

--Lo sig. no es necesario ya que Haskell lo establece automáticamente.
--Zero :: Nat
--Succ :: Nat -> Nat

--Ejerc. 2) Definir la función natToInt : Nat → Int que dado un número Nat retorna
--su entero correspondiente. Por ejemplo: natToInt (Suc(Suc Zero)) = 2.

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n         

--3. Definir la función intToNat : Int → N at que dado un número entero retorna
--su Nat correspondiente. Por ejemplo: intToNat 2 = (Suc(Suc Zero)).
--IMPORTANTE: Succ (intToNat (n-1)) ANALIZAR PORQUE SON NECESARIOS LOS PARÉNTESIS, recordar que Succ funciona como una función y que sin los paréntesis se generaría un ciclo infinito.

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n-1))

--4. Definir la función sumaNat : Nat → Nat → Nat, la cual suma dos números Nat.

sumaNat :: Nat -> Nat -> Nat
sumaNat Zero Zero = Zero
sumaNat x y = intToNat (natToInt x + natToInt y)

--5. Definir los árboles binarios.

data TreeAll a = Nil | Node (TreeAll a) a (TreeAll a) 

--Definir las siguientes funciones sobre árboles binarios: size y height
--6. La función size, que dado un árbol retorna el número de nodos del árbol. 

size :: Tree a -> Int
size Nil = 0
size (Node hi r hd) = 1 + size hi + size hd

--7. La función height, que dado un árbol retorna la altura del mismo.
height :: Tree a -> Int
height Nil = 0
height (Node (hi) a (hd))= 1 +  max (height (hd)) (height (hd))

