
potencia x 0 = 1
potencia x y = x*potencia x (y-1)

nand True True = False
nand _ _ = True

nor False False = True
nor _ _ = False

absoluto n
 | n>= 0 = n
 | otherwise = -n

cobroLuz n
 | n <= 150 = 0.912*n
 | n <= 280 = 136.80+(n-150)*1.111
 | n > 280 = 281.23 + (n-280)*3.248

cfeIva m
 | m > 0 = (cobroLuz m)*0.16 + cobroLuz m

euclides x 0 = x
euclides x y = euclides y (mod x y)

mcm a b = div (a*b) (euclides a b)

--Funcion resta
resta 0 m = 0
resta n 0 = n
resta n m = resta (n - 1) (m - 1)
--Funcion divide
divide n m = if (n < m) then 0 else 1 + divide (resta n m) m 
--Funcion modulo
modulo n m = if (n < m) then n else modulo (resta n m) m

esPrimo 0 = False
esPrimo 1 = False
esPrimo 2 = True
esPrimo x = not (divisible x (x-1))

divisible x 1 = False
divisible x n = if mod x n == 0 then True else divisible x (n-1)

prueba x n = mcm x (n-1)

abundante x = prueba x x