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

--ACT. 2
--Sea xs un lista no vacía con elementos booleanos, tal que true aparezca al menos una vez en la lista. Especificar:

--n es el menor entero tal que xs.n = true.




