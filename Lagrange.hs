module Lagrange where

--lpol racuna proizvod od j=0 do n, j/=i, (x - xj)/(xi - xj) koji ce nam biti potreban za Lagranzov polinom

lpol :: Double -> [Double] -> Double -> Double -> Double
lpol a [x] xi p |xi==x = p
                |otherwise = p*((a-x)/(xi-x))
                      
lpol a (x:xs) xi p | xi==x = lpol a xs xi p
                   |otherwise =  lpol a xs xi (p*((a-x)/(xi-x)))
                    

--lagrange i pomocna funkcija lagr racunaju vrednost funkcije u tacki a primenom Lagranovog interpolacionog polinoma
--a - tacka u kojoj zelimo da odredimo vrednost funkcije
--x - lista cvorova
--f - lista vrednosti funkcije u cvorovima
                    
lagrange :: Double -> [Double] -> [Double] -> Double
lagrange a x f = lagr a x f x                     


lagr :: Double -> [Double] -> [Double] -> [Double] -> Double
lagr a [] _ l = 0                    
lagr a (x:xs) (f:fs)  l= ((lagr a xs fs l)+ (f* (lpol a l x 1)))


--inverz racuna vrednost funkcije u tacki a primenog inverznog Lagranzovog interpolacionog polinoma

inverz :: Double -> [Double] -> [Double] -> Double
inverz a x f = lagrange a f x

