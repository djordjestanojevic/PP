module Simple where

--Funkcije ekvidistantni i ekvidistantni1 proveravaju da li su cvorovi unutar liste cvorova x ekvidistantni.

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
    

--isdecreasing i isincreasing proveravaju da li su cvorovi unutar liste uredjeni opadajuce, odnosno rastuce.

isdecreasing :: [Double] -> Bool
isdecreasing [x] = True
isdecreasing [x,y] |x>y = True
                   |otherwise = False

isdecreasing (x:y:xs) |x>y  && (isdecreasing (y:xs)) = True
                      |otherwise = False


isincreasing :: [Double] -> Bool
isincreasing [x] = True
isincreasing [x,y] |x<y = True
                   |otherwise = False

isincreasing (x:y:xs) | (isincreasing (y:xs)) && x<y = True
                      | otherwise = False


--ismonotonic koristeci isincreasing i isdecreasing proverava da li je funkcija monotona
                      
ismonotonic :: [Double] -> Bool
ismonotonic x | (isincreasing x) || (isdecreasing x) = True
              |otherwise = error "Funkcija nije monotona"
 
 
 --factorial racuna faktorijel broja n
              
factorial :: Int -> Double
factorial n
    | n == 0 = 1.0
    | otherwise = (fromIntegral n) * factorial (n-1)


--konacne racuna konacnu razliku odredjenog reda
    
konacne :: [Double] -> Int -> Double
konacne [f] 0 = f

konacne [f1, f2] 1 = f2 - f1

konacne f red = (konacne (drop 1 f) (red-1))-(konacne (take red f)(red-1))
