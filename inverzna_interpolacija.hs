--TODO: skratiti interval za niz x

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

interval :: Double -> [Double] -> [Double]
interval a [f1, f2] = [f1,f2]
interval a [] = error "Prosledili ste praznu listu!"
interval a [f] = error "Neispravni argumenti!"
interval a (f1:f2:fs) 
    |a < f1 && a > f2 = [f1, f2] --za slucaj da je fja opadajuca
    |a > f1 && a < f2 = [f1, f2] --za slucaj da je fja rastuca
    |otherwise = interval a (f2:fs)
  
isdecreasing :: [Double] -> Bool  
isdecreasing [x] = True
isdecreasing [x,y] |x>y = True
                   |otherwise = False
isdecreasing (x:y:xs) |x>y  && (isdecreasing (y:xs)) = True
                      | otherwise = False

isincreasing :: [Double] -> Bool
isincreasing [x] = True
isincreasing [x,y] |x<y = True
                   |otherwise = False
isincreasing (x:y:xs) | (isincreasing (y:xs)) && x<y = True
                      | otherwise = False

ismonotonic :: [Double] -> Bool
ismonotonic x | (isincreasing x) || (isdecreasing x) = True
              | otherwise = False

monotonInterval1 :: [Double] -> Double -> [Double] 
monotonInterval1 f f_x = monotonInterval f (interval f_x f)
              
monotonInterval :: [Double] -> [Double] -> [Double]
monotonInterval f suzenInterval 
    |head (suzenInterval) == (f !! 0) = idiDesno f suzenInterval
    |suzenInterval !! ((length suzenInterval) - 1) == f !! ((length f) - 1) = idiLevo f suzenInterval
    |otherwise = (idiLevo (take  (func1 (reverse f) (head (suzenInterval))) f) suzenInterval) ++ (drop (length suzenInterval)(idiDesno (drop(length suzenInterval +(func1 (reverse f) (head (suzenInterval)))) f) suzenInterval) )

func1 :: [Double] -> Double -> Int
func1 rev_f x
    | head (rev_f) == x = (length rev_f - 1)
    |otherwise = func1 (tail rev_f) x

idiDesno :: [Double] -> [Double] -> [Double]
idiDesno [] suzenInterval = suzenInterval
idiDesno (f:fs) suzenInterval 
    |(ismonotonic (suzenInterval ++ [f])) = idiDesno fs (suzenInterval ++ [f])
    |otherwise = suzenInterval
    
idiLevo :: [Double] -> [Double] -> [Double]
idiLevo [] suzenInterval = suzenInterval
idiLevo f suzenInterval 
    |(ismonotonic (f!!(length f -1):suzenInterval)) = idiLevo (take (length f - 1) f) (f!!(length f -1):suzenInterval)
    |otherwise = suzenInterval
   
niz_konacne :: [Double] -> Int -> [Double]
niz_konacne f red 
    |red == 0 = [f !! 0]
    |red == 1 = (niz_konacne (take red f) (red-1)) --zato sto nam kr 1. reda ne trebaju za racunanje fi(q)
    |otherwise = (niz_konacne (take red f) (red-1)) ++ [konacne f red]

inverzna1 :: Int -> [Double] -> Double -> Double -> Double -> Double
inverzna1 red f q eps f_x = inverzna red (monotonInterval1 f f_x) q eps f_x
    
inverzna :: Int -> [Double] -> Double -> Double -> Double -> Double
inverzna red f q eps f_x
    |abs ((fi q red f f_x) - q) < eps = (fi q red f f_x)
    |otherwise = fi (fi q red f f_x) red f f_x

izracunajX :: Int -> [Double] -> [Double] -> Double -> Double -> Double  
izracunajX red f x eps f_x = (inverzna red f 0 eps f_x)*(x!!1 - x!!0) + (x!!0)
  
fi :: Double -> Int -> [Double] -> Double -> Double
fi q red f f_x = ((-1)/(konacne [f!!0,f!!1] 1))*(fi1 q red (reverse(niz_konacne f red)) f_x)

fi1 :: Double -> Int -> [Double] -> Double -> Double
fi1 q red niz f_x
    |niz == [] = 0
    |length niz == 1 = head niz - f_x
    |otherwise = (head niz)*(pom q red)/(factorial red) + fi1 q (red - 1) (tail niz) f_x 

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
