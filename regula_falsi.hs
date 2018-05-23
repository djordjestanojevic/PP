import Simple as S
import Diferenciranje as D

   
--pravi sledeci clan niza
niz :: (Double -> Double) -> Double -> Double -> Double 
niz f x0 xn  = (xn-((x0-xn)*(f xn)/((f x0)-(f xn)))) 

--pronalazi nulu eksplicitno zadate funkcije f sa tacnoscu e koriscenjem metode laznog polozaja
nula :: (Double -> Double) -> Double -> Double -> Double -> Double
nula f a b e|(f a)*(f b) > 0 = error "Funkcija nije lokalizovana"
            |S.ismonotonic (map (f) niz) == False = error "Funkcija nije monotona"
            |S.ismonotonic (niz_izvoda niz (map f niz) niz) == False = error "Drugi izvod nije konstantan"
            |(f b)*(D.izvod b niz (niz_izvoda niz (map f niz) niz))>0 = nula_funkcije f b a e
            |otherwise = nula_funkcije f a b e
    where niz = [a,a+(b-a)/10..b]
                   
nula_funkcije :: (Double -> Double) -> Double -> Double -> Double -> Double
nula_funkcije f a b e |abs(f b) < e = b
                      |otherwise = nula_funkcije f a (niz f a b) e

--pravi niz vrednosti izvoda funkcije f
niz_izvoda::[Double]->[Double]->[Double]->[Double]
niz_izvoda [] _ _ = []
niz_izvoda x f x0 = (D.izvod (head x) x0 f):(niz_izvoda (tail x) f x0)
