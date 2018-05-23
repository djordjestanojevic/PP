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

