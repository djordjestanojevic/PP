import Simple as S


--pronalazi nulu eksplicitno zadate funkcije f na intervalu (a,b) sa tacnoscu e koriscenjem metode polovljenja intervala
nula_funkcije :: (Double -> Double) -> Double -> Double -> Double -> Double
nula_funkcije f a b e |S.ismonotonic (map (f) [a,a+(b-a)/10..b]) == False = error "Funkcija nije monotona"
                      |f b == 0 = b
                      |f a == 0 = a
                      | abs(b-a)<e = b
                      | (f b) * (f ((a+b)/2)) < 0  = nula_funkcije f ((a+b)/2) b e
                      |otherwise = nula_funkcije f a ((a+b)/2) e

                      
                      
