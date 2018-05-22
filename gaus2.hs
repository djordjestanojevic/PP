ekvidistantni :: [Double] -> Bool
ekvidistantni [x] = error "Neispravni argumenti"
ekvidistantni [x1, x2] = True
ekvidistantni (x1:x2:xs) = ekvidistantni1 (x2:xs) (abs (x1 - x2))

ekvidistantni1 :: [Double] -> Double -> Bool
ekvidistantni1 [a, b] razlika 
    | abs (a-b) == razlika = True
    |otherwise = False
ekvidistantni1 (x2:x3:xs) razlika
    | abs (x2 - x3) /= razlika = False
    | otherwise = ekvidistantni1 (x3:xs) razlika

konacne :: [Double] -> Int -> Double
konacne [f] 0 = f
konacne [f1, f2] 1 = f2 - f1
konacne f red = (konacne (drop 1 f) (red-1))-(konacne (take red f)(red-1))


napravi_f :: [Double] -> Int -> Int -> [Double]
napravi_f f indeks k | k==0 = [f!!indeks]
                     | odd k = (f!!(indeks-(div (k+1) 2))):(napravi_f f indeks (k-1))
                     | otherwise = (napravi_f f indeks (k-1))++[f!!(indeks+(div k 2))]
                     
lista_f f indeks 0 = [(napravi_f f indeks 0)]
lista_f f indeks k = (lista_f f indeks (k-1))++[napravi_f f indeks k]

pom:: Double->Int->Double 

pom q k | odd k = pom1 q (div k 2)
        | even k = (pom1 q (div (k-1) 2))*(q+fromIntegral (k-1))

pom1::Double->Int->Double
pom1 q 0 = q

pom1 q k = (q*q-((fromIntegral(k)-1)*(fromIntegral(k)-1)))*(pom1 q (fromIntegral(k)-1))


gaus:: Double->[Double]->[Double]->Int->Double
gaus a x f n | ekvidistantni x = gaus1 (((a-(x!!(div n 2)))/(x!!1-x!!0))-1) (map (\c->(konacne c (length c-1)))(reverse(lista_f f (div (n+1) 2) n))) n
             | otherwise = error "Nisu ekvidistantni! \n"

factorial :: Int -> Double
factorial n
    | n == 0 = 1.0
    | otherwise = (fromIntegral n) * factorial (n-1)

gaus1:: Double->[Double]->Int->Double
gaus1 _ kon 0 = kon!!0
gaus1 q kon n = (gaus1 q (drop 1 kon) (fromIntegral(n)-1))+((kon!!0)*(pom q n)/(factorial n))