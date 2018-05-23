import Simple as S
pom :: Double -> Int -> Double
pom q n = pom1 q n 0

pom1 :: Double -> Int -> Int -> Double
pom1 q n i 
    |i == n = 1
    | otherwise = (q + (fromIntegral i)) * pom1 q n (i+1)

njutn2 :: Double -> [Double] -> [Double] -> Int -> Double
njutn2 a x f n 
    | S.ekvidistantni x /= True = error "Cvorovi nisu ekvidistantni"
    | otherwise = njutn2pom a (reverse f) n ((a - x !! ((length x) -1))/(abs(x !! 1 - x !! 0)))
  
njutn2pom :: Double -> [Double] -> Int -> Double -> Double
njutn2pom a f n q
    |n == 0 = S.konacne f 0
    |otherwise = ((S.konacne (take (n+1) f) n)/(S.factorial n))*(pom q n) + njutn2pom a (take n f) (n-1) q  
    
    
