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
              | otherwise = error "Funkcija nije monotona"

              
proveri :: Double -> Double -> (Double -> Double) -> Bool             
proveri a b f |a<b =  ismonotonic (map (f) [a,a+(b-a)/10..b])
              |otherwise = ismonotonic (map (f) [b,b+(b-a)/10..a])
   
   
niz :: (Double -> Double) -> Double -> Double -> Double -> Double
niz f x0 xn e = nula_funkcije f x0 (xn-((x0-xn)*(f xn)/((f x0)-(f xn)))) e 


nula :: (Double -> Double) -> Double -> Double -> Double -> Double
nula f a b e|(proveri a b f == False) = error "Funkcija nije monotona"
            |otherwise = nula_funkcije f a b e
                   
                   
nula_funkcije :: (Double -> Double) -> Double -> Double -> Double -> Double
nula_funkcije f a b e |f b == 0 = b
                      |f a == 0 = a
                      | abs(b-a)<e = b 
                      |otherwise = niz f a b e
