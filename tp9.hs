--PROG. AVANZADA - PRÁCT.9: ESPECIFICACIONES.  ∃

--ACT.1

--f es una función que determina si los elementos de una lista xs son iguales.
--Considero que length cuenta desde cero.
elmsEq: [Int] -> Bool
elmsEq.xs = <∀i: 0 <= i < #xs - 1: xs.i == xs. (i+1)>
elmsEq.xs = <∀i: 0 <= i < (length xs - 1): xs.i == xs. (i+1)>

--f es una función que determina si los elementos de una lista xs son todos diferentes.
--Considero que length cuenta desde cero.
elmsDist: [Int] -> Bool

"ESTA ESPECIFICACIÓN SOLO COMPRUEBA SI EL SUCESOR ES DISTINTO"
elmsDist.xs = <∀i: 0 <= i < #xs - 1: xs.i /= xs. (i+1)>
elmsDist.xs = <∀i: 0 <= i < (length xs - 1): xs.i /= xs. (i+1)>

"ESTA ESPECIFICACIÓN CUMPLE LO SOLICITADO"
elmsDist.xs = <∀i,j: 0 <= i < #xs - 1 && i < j < #xs: xs.i /= xs.j>

--f es una función que determina si los elementos de una lista xs están ordenados.
elmsOrd: [Int] -> Bool
elmsOrd.xs = <∀i: 0 <= i < #xs-1: xs.i <= xs.(i + 1)>
elmsOrd.xs = <∀i,j: 0 <= i < #xs - 1 && i < j < #xs: xs.i <= xs.j>
elmsOrd.xs = <∀i,j: 0 <= i < (length xs - 1) && i < j < length xs: xs.i <= xs.j>

--P es un predicado que es true sii cuando aparece 1 en xs entonces debe aparecer 0 en xs.
--IMPORTANTE: Las
onezero: [Int] -> Bool
onezero.xs = <∃i,j: 0 <= i < #xs && 0 <= j < #xs: xs.i == 1 && xs.j == 0>

--p es el producto de todos los elementos primos de xs.
prodPrims : [Int] -> Int
prodPrims.xs = <∏i: 0 <= i < #xs && divs (xs.i) = 2: xs.i>
--SI ME DEVUELVE 2, PUEDO PLANTEAR QUE n ES PRIMO.
	where divs: Int -> Int
	      divs.x = <Contatoria i: 1 <= i <=x && mod i x = 0: i>

--ACT.2
--Sea xs un lista no vacía con elementos booleanos, tal que true aparezca al menos una vez en la lista. Especificar:

--CONTROLAR EN TODAS SI EL TÉRMINO ES CORRECTO
--n es el menor entero tal que xs.n = true.
menor : [Int] -> Int
menor.xs = <Min i: 0 <= i <= #xs && xs.i = True: i>

--n es el último elemento de la lista tal que xs.n = true.
ult : [Int] -> Int
ult.xs = <∃i: 0 <= i <= #xs && i = #xs: xs.i>
ult.xs = <Max i: 0 <= i <= #xs && xs.i: i>

--REVISAR
--f es una función que devuelve true si y solo si todos los elementos de xs son equivalentes.
allEq : [Int] -> Bool
allEq.xs = <∀i,j: 0 <= i <= #xs && 0 <= j < #xs: xs.i = xs.j>

--ACT.3
--REVISAR
--f.xs determina si xs tiene la misma cantidad de pares que impares.
paresImprs: [Int] -> Bool
paresImprs.xs = <∀i:i = #xs-1 && (cantPares xs.i = cantImpares xs.i): xs.i> 
paresImprs.xs = (<∏i: 0 <= i <= #xs && mod xs.i 2 = 0: xs.i>) = (<∏i: 0 <= i <= #xs && mod xs.i 2 /= 0: xs.i>)

cantPares : [Int] -> Int
<∏i: 0 <= i <= #xs && mod xs.i 2 = 0: xs.i>

cantImpares : [Int] -> Int
<∏i: 0 <= i <= #xs && mod xs.i 2 /= 0: xs.i>

--f.n determina si n es primo.
prime: Int -> Bool
prime.n = (<Contatoria i: 1 <= i <= n && mod i x = 0: i> == 2)
prime.n = (<Contatoria i: 1 <= i <= n: mod i x = 0> == 2)

SE FILTRA EL TÉRMINO
--f.xs.ys determina si ys es una subsecuencia de xs.
subsec : [a] -> [a] -> Bool
subsec.xs.ys = <∃as,bs : xs = as ++ ys ++ bs>
subsec.xs.ys = <∃as,bs: BUSCAR OTRA FORMA

SE FILTRA EL TÉRMINO
--f.xs.ys determina si ys es una subsecuencia final de xs.
subsecFin : [a] -> [a] -> Bool
subsecFin.xs.ys = <∃as : xs = as ++ ys>

--ACT.4
--Dada una lista de enteros, especifique la suma del subsegmento de suma mínima de la lista. Por ejemplo, si la lista es xs = [1, −4, −2, --1, −5, 8, −7] el subsegmento que da la suma mínima es [−4, −2, 1, −5], cuya suma es -10. Si xs = [1, 3, 5] , 
--el subsegmento que da la suma mínima es [] , pues la suma de la lista vacía es cero.

REHACERLA SOLO
sumMin : [Int] -> 
sumMin.xs = <Min as,bs,cs: as ++ bs ++ cs == xs: <SUMATORIA i: i <= 0 < #xs: xs.i> >

--Especifique la funcin maxigual que determina la longitud del m ́aximo sub-segmento en donde todos sus elementos son iguales: maxigual : [A] −> Num.
REHACERLA SOLO



--Especifique la funci ́on maxdistinto : [Int]− > Int que determina la longitud del subsegmento m ́as largo en donde todos los elementos son distintos.




