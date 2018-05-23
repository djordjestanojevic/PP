module Gaus1 where
import Simple as S


napravi_f :: [Double] -> Int -> Int -> [Double]
napravi_f f indeks k | k==0 = [f!!indeks]
                     | even k = (f!!(indeks-(div k 2))):(napravi_f f indeks (k-1))
                     | otherwise = (napravi_f f indeks (k-1))++[f!!(indeks+(div (k+1) 2))]

                     
lista_f :: [Double] -> Int -> Int -> [[Double]]
lista_f f indeks 0 = [(napravi_f f indeks 0)]
lista_f f indeks k = (lista_f f indeks (k-1))++[napravi_f f indeks k]


pom:: Double->Int->Double 
pom q k | odd k = pom1 q (div k 2)
        | even k = (pom1 q (div (k-1) 2))*(q-fromIntegral (k-1))

        
pom1::Double->Int->Double
pom1 q 0 = q
pom1 q k = (q*q-((fromIntegral(k)-1)*(fromIntegral(k)-1)))*(pom1 q (fromIntegral(k)-1))


gaus:: Double -> [Double] -> [Double] -> Int -> Double
gaus a x f n | S.ekvidistantni x = gaus1 ((a-(x!!(div n 2)))/(x!!1-x!!0)) (map (\c->(S.konacne c (length c-1)))(reverse(lista_f f (div n 2) n))) n
             | otherwise = error "Nisu ekvidistantni! \n"

    
gaus1 :: Double->[Double]->Int->Double
gaus1 _ kon 0 = kon!!0
gaus1 q kon n = (gaus1 q (drop 1 kon) (fromIntegral(n)-1))+((kon!!0)*(pom q n)/(S.factorial n))
