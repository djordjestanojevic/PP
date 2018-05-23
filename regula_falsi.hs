import Simple as S
import Diferenciranje as D

proveri :: Double -> Double -> (Double -> Double) -> Bool             
proveri a b f |a<b =  S.ismonotonic (map (f) [a,a+(b-a)/10..b])
              |otherwise = S.ismonotonic (map (f) [b,b+(b-a)/10..a])
   
   
niz :: (Double -> Double) -> Double -> Double -> Double 
niz f x0 xn  = (xn-((x0-xn)*(f xn)/((f x0)-(f xn)))) 


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

                      
                               
                               
niz_izvoda [] _ _ = []
niz_izvoda x f x0 = (D.izvod (head x) x0 f):(niz_izvoda (tail x) f x0)
