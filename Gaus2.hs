module Gaus2 where
import Simple as S
--funkcija pravi listu cvorova u redu kojim treba da se dodaju
napravi_f :: [Double] -> Int -> Int -> [Double]
napravi_f f indeks k | k==0 = [f!!indeks]
                     | odd k = (f!!(indeks-(div (k+1) 2))):(napravi_f f indeks (k-1))
                     | otherwise = (napravi_f f indeks (k-1))++[f!!(indeks+(div k 2))]
--pravi listu listi cvorova kako bi olaksalo dalje izracunavanje
lista_f f indeks 0 = [(napravi_f f indeks 0)]
lista_f f indeks k = (lista_f f indeks (k-1))++[napravi_f f indeks k]

pom:: Double->Int->Double 

pom q k | odd k = pom1 q (div k 2)
        | even k = (pom1 q (div (k-1) 2))*(q+fromIntegral (k-1))

--racuna q*...(q*q-n*n)  
pom1::Double->Int->Double
pom1 q 0 = q

pom1 q k = (q*q-((fromIntegral(k)-1)*(fromIntegral(k)-1)))*(pom1 q (fromIntegral(k)-1))

--funkcija izracunava vrednost funkcije f zadate vrednosti u n+1 tacki pomocu drugog Gausovog interpolacionog polinoma
gaus:: Double->[Double]->[Double]->Int->Double
gaus a x f n | S.ekvidistantni x = gaus1 (((a-(x!!(div n 2)))/(x!!1-x!!0))-1) (map (\c->(S.konacne c (length c-1)))(reverse(lista_f f (div (n+1) 2) n))) n
             | otherwise = error "Nisu ekvidistantni! \n"

gaus1:: Double->[Double]->Int->Double
gaus1 _ kon 0 = kon!!0
gaus1 q kon n = (gaus1 q (drop 1 kon) (fromIntegral(n)-1))+((kon!!0)*(pom q n)/(S.factorial n))
