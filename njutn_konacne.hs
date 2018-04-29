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

pom :: Double -> Int -> Double
pom q n = pom1 q n 0

pom1 :: Double -> Int -> Int -> Double
pom1 q n i 
    |i == n = 1
    | otherwise = (q - (fromIntegral i)) * pom1 q n (i+1)

factorial :: Int -> Double
factorial n
    | n == 0 = 1.0
    | otherwise = (fromIntegral n) * factorial (n-1)

njutn1 :: Double -> [Double] -> [Double] -> Int -> Double
njutn1 a x f n 
    | ekvidistantni x /= True = error "Cvorovi nisu ekvidistantni"
    | otherwise = njutn1pom a f n ((a - x !! 0)/(abs(x !! 1 - x !! 0)))
  
njutn1pom :: Double -> [Double] -> Int -> Double -> Double
njutn1pom a f n q
    |n == 0 = konacne f 0
    |otherwise = ((konacne (take (n+1) f) n)/(factorial n))*(pom q n) + njutn1pom a (take (n) f) (n-1) q  