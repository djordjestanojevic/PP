module Inverzna_interpolacija where
import Simple as S

--pravi niz konacnih razlika u redu koji olaksava dalje izracunavanje
niz_konacne :: [Double] -> Int -> [Double]
niz_konacne f red 
    |red == 0 = [f !! 0]
    |red == 1 = (niz_konacne (take red f) (red-1)) --zato sto nam kr 1. reda ne trebaju za racunanje fi(q)
    |otherwise = (niz_konacne (take red f) (red-1)) ++ [S.konacne f red]

--proverava zadovoljenost uslova zaustavljanja pravljenja iterativnog niza
inverzna :: Int -> [Double] -> Double -> Double -> Double -> Double
inverzna red f q eps f_x
    |abs (qn - q) < eps = qn
    |otherwise = fi qn red f f_x
    where qn = (fi q red f f_x)

    
--izracunava vrednost x za koje je f(x)=f_x koriscenjem inverzne interpolacije
izracunajX :: Int -> [Double] -> [Double] -> Double -> Double -> Double  
izracunajX red f x eps f_x = (inverzna red f 0 eps f_x)*(x!!1 - x!!0) + (x!!0)
--kontrakcija koja pravi iterativni niz
fi :: Double -> Int -> [Double] -> Double -> Double
fi q red f f_x = ((-1)/(S.konacne [f!!0,f!!1] 1))*(fi1 q red (reverse(niz_konacne f red)) f_x)

fi1 :: Double -> Int -> [Double] -> Double -> Double
fi1 q red niz f_x
    |niz == [] = 0
    |length niz == 1 = head niz - f_x
    |otherwise = (head niz)*(pom q red)/(S.factorial red) + fi1 q (red - 1) (tail niz) f_x 
--racuna q*(q-1)..*(q-n+1)
pom :: Double -> Int -> Double
pom q n = pom1 q n 0

pom1 :: Double -> Int -> Int -> Double
pom1 q n i 
    |i == n = 1
    | otherwise = (q - (fromIntegral i)) * pom1 q n (i+1)
    
