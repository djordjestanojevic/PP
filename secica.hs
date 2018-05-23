import Simple as S              
proveri :: Double -> Double -> (Double -> Double) -> Bool
proveri a b f |a<b =  S.ismonotonic (map (f) [a,a+(b-a)/10..b])
              |otherwise = S.ismonotonic (map (f) [b,b+(b-a)/10..a])
              
          
niz :: (Double -> Double) -> Double -> Double -> Double -> Double          
niz f x0 xn e = nula_funkcije f x0 (xn-((x0-xn)*(f xn)/((f x0)-(f xn)))) e 


nula :: (Double -> Double) -> Double -> Double -> Double -> Double
nula f a b e|proveri a b f == False = error "Funkcija nije monotona"
            |otherwise = nula_funkcije f a b e
                      

nula_funkcije :: (Double -> Double) -> Double -> Double -> Double -> Double
nula_funkcije f a b e |f b == 0 = b
                      | abs(b-a)<e = b 
                      |otherwise = niz f a (niz f a b e) e
