sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta _ 1 = 1
sumaDivisoresHasta n x | mod n x == 0 = x + sumaDivisoresHasta n (x-1)
                       | otherwise = sumaDivisoresHasta n (x-1)

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n


menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n m | mod n m == 0 = m
                      | otherwise = menorDivisorDesde n (m+1)
                      
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2 

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n | menorDivisor n == n = True
          | otherwise = False
          
nEsimoPrimo :: Int -> Int

nEsimoPrimoDesde :: Int -> Int -> Int
nEsimoPrimoDesde 1 m | esPrimo m = m
		     | otherwise = nEsimoPrimoDesde 1 (m+1)
		     
nEsimoPrimoDesde n m | esPrimo m = nEsimoPrimoDesde (n-1) (m+1)
                     | otherwise = nEsimoPrimoDesde n (m+1)
                     
nEsimoPrimo n = nEsimoPrimoDesde n 2

-- otra forma de pensarlo: n-esimo primo = siguiente primo ((n-1)-esimo primo)
sigPrimo :: Int -> Int
sigPrimo n | esPrimo n = n
           | otherwise = sigPrimo (n+1)
           
nEPrimo :: Int -> Int
nEPrimo 1 = 2
nEPrimo n = sigPrimo (nEPrimo (n-1) + 1)

nEsimoFibo :: Int -> Int
nEsimoFibo 0 = 1
nEsimoFibo 1 = 1
nEsimoFibo n = nEsimoFibo (n-1) + nEsimoFibo (n-2)

primerFiboMayorOIgual :: Int -> Int -> Int
primerFiboMayorOIgual n i | curFib >= n = curFib
                          | otherwise = primerFiboMayorOIgual n (i+1)
                          where curFib = nEsimoFibo i
                          
esFibonacci :: Int -> Bool
esFibonacci n = (primerFiboMayorOIgual n 0) == n

sumaPrimerosNPrimos :: Int -> Int
sumaPrimerosNPrimos 1 = 2
sumaPrimerosNPrimos n = nEsimoPrimo n + sumaPrimerosNPrimos (n-1)

primerSumaMayorOIgual n i | curSuma >= n = curSuma
                          | otherwise = primerSumaMayorOIgual n (i+1)
                          where curSuma = sumaPrimerosNPrimos i
                          
esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = (primerSumaMayorOIgual n 1) == n
