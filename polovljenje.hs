import Simple as S
proveri :: Double -> Double ->(Double -> Double)-> Bool
proveri a b f =  S.ismonotonic (map (f) [a,a+(b-a)/10..b])

nula_funkcije :: (Double -> Double) -> Double -> Double -> Double -> Double
nula_funkcije f a b e |proveri a b f == False = error "Funkcija nije monotona"
                      |f b == 0 = b
                      |f a == 0 = a
                      | abs(b-a)<e = b
                      | (f b) * (f ((a+b)/2)) < 0  = nula_funkcije f ((a+b)/2) b e
                      |otherwise = nula_funkcije f a ((a+b)/2) e

                      
                      
