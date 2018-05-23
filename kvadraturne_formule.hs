pravougaonik :: Double -> Double -> Double -> (Double -> Double) -> Double
pravougaonik a b h f = (foldr (+) 0 (map (f) [a+h/2,a+3*h/2..b-h/2]))


trapez :: Double -> Double -> Double -> (Double -> Double) -> Double
trapez a b h f = ((f a) + (f b) + 2* (foldr (+) 0 (map (f) [a+h,a+2*h..b-h]))*h/2)


simpson :: Double -> Double -> Double -> (Double -> Double) -> Double
simpson a b h f = ((f a) + (f b) + 2*(foldr (+) 0 (map (f) [a+2*h,a+4*h..b-2*h]))+ 4*(foldr (+) 0 (map (f) [a+h,a+3*h..b-h])))*h/3
