module Njutn_podeljene where 


--podeljene_razlike racuna podeljenu razliku odredjenog reda

podeljene_razlike :: [Double] -> [Double] -> Int -> Double
podeljene_razlike [x] [f] 0 = f

podeljene_razlike [x1, x2] [f1, f2] 1 = (f2 - f1)/(x2 - x1)

podeljene_razlike x f red
    | ((length f - 1) /= red)  || ((length x - 1) /= red) = error "Neispravni argumenti" 
    |otherwise = (podeljene_razlike (drop 1 x) (drop 1 f) (red-1) - podeljene_razlike (take ((length x)-1) x) (take ((length f)-1) f) (red-1))/ (x !! ((length x)-1) - x !! 0)

  
--pom  je pomocna funkcija koja racuna deo iz Njutnovog interpolacionog polinoma: (x-x0)*(x-x1)*(x-x2)*...*(x-xn-1) 
  
pom :: Double -> [Double] -> Int -> Double
pom _ [x] _ = 1
pom a (x:xs) n = (a - x) * pom a xs (n-1)


--njutn izracunava rezultat koriscenjem Njutnovog interpolacionog polinoma sa podeljenim razlikama
--koristeci funkcije podeljene_razlike i pom 

njutn :: Double -> [Double] -> [Double] -> Int -> Double
njutn a x f n 
    | n == 0 = podeljene_razlike x f 0
    |otherwise = njutn a (take n x) (take n f) (n-1) + (podeljene_razlike x f n)*(pom a x (n-1))
 

--njutn_inverzno izracunava rezultat primenom inverznog Njutnovog inteprolacionog polinoma sa podeljenim razlikama 
 
njutn_inverzno :: Double -> [Double] -> [Double] -> Int -> Double
njutn_inverzno a x f n = njutn a (reverse x) (reverse f) n
