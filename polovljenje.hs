
isdecreasing [x] = True
isdecreasing [x,y] |x>y = True
                   |otherwise = False

isdecreasing (x:y:xs) |x>y  && (isdecreasing (y:xs)) = True
                      | otherwise = False



isincreasing [x] = True
isincreasing [x,y] |x<y = True
                   |otherwise = False

isincreasing (x:y:xs) | (isincreasing (y:xs)) && x<y = True
                      | otherwise = False



ismonotonic x | (isincreasing x) || (isdecreasing x) = True
              | otherwise = error "Funkcija nije monotona"

              


proveri a b f =  ismonotonic (map (f) [a,a+(b-a)/10..b])

nula_funkcije f a b e |proveri a b f == False = error "Funkcija nije monotona"
                      |f b == 0 = b
                      |f a == 0 = a
                      | abs(b-a)<e = b
                      | (f b) * (f ((a+b)/2)) < 0  = nula_funkcije f ((a+b)/2) b e
                      |otherwise = nula_funkcije f a ((a+b)/2) e

                      
                      
