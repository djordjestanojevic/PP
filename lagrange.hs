polyval :: [Double] -> Double -> Double
polyval lista x = polynomial (reverse lista) x


polynomial :: [Double] -> Double -> Double
polynomial [a] x = a
polynomial (a:xs) x = (polynomial xs x)*x + a


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


ismonotonic :: [Double] -> Bool
ismonotonic x | (isincreasing x) || (isdecreasing x) = True
              |otherwise = error "Funkcija nije monotona"

lpol :: Double -> [Double] -> Double -> Double -> Double
lpol a [x] xi p |xi==x = p
                |otherwise = p*((a-x)/(xi-x))
                      
lpol a (x:xs) xi p | xi==x = lpol a xs xi p
                   |otherwise =  lpol a xs xi (p*((a-x)/(xi-x)))
                    
                
lagrange :: Double -> [Double] -> [Double] -> Double
lagrange a x f = lagr a x f x                     


lagr :: Double -> [Double] -> [Double] -> [Double] -> Double
lagr a [] _ l = 0                    
lagr a (x:xs) (f:fs)  l= ((lagr a xs fs l)+ (f* (lpol a l x 1)))


inverz :: Double -> [Double] -> [Double] -> Double
inverz a x f = lagrange a f x

