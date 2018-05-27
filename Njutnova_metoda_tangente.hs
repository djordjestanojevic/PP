module Njutnova_metoda_tangente where
import Diferenciranje as D
import Simple as S
--pronalazi nulu eksplicitno zadate funkcije f na intervalu a b sa tacnoscu eps
njutn_resenje:: Double->Double->(Double->Double)->Double->Double
njutn_resenje a b f eps |(f a)*(f b) > 0 = error "Funkcija nije lokalizovana"
                        |S.ismonotonic (map (f) niz) == False = error "Funkcija nije monotona"
                        |S.ismonotonic (niz_izvoda niz (map f niz) niz) == False = error "Drugi izvod nije konstantan"
                        |(f b)*(D.izvod b niz (niz_izvoda niz (map f niz) niz))>0 = njutn_pom a b f eps b
                        |otherwise = njutn_pom a b f eps a
                        where niz = [a,a+(b-a)/10..b]

--funkcija sa zadatim pocetnim cvorom
njutn_pom:: Double->Double->(Double->Double)->Double->Double->Double
njutn_pom a b f eps xn | abs(r)<eps = xn-r
                       | otherwise = njutn_pom a b f eps (xn - r)
    where r= ((f xn)/(D.izvod xn ([a,a+((b-a)/10)..b]) (map f ([a,a+((b-a)/10)..b]))))

--modifikacija njutnove metode tangente
modifikacija:: Double->Double->(Double->Double)->Double->Int->Double
modifikacija a b f eps k = mod_pom a b f eps b k 0 0


mod_pom::  Double->Double->(Double->Double)->Double->Double->Int->Int->Double->Double
mod_pom a b f eps xn k 0 i | abs((f xn)/(iz))<eps =xn
                             | otherwise = mod_pom a b f eps (xn - ((f xn)/(iz))) k k (iz)
    where iz = D.izvod xn ([a,a+((b-a)/10)..b]) (map f ([a,a+((b-a)/10)..b]))
mod_pom a b f eps xn k p i | abs(((f xn)/(i)))<eps =xn
                             | otherwise = mod_pom a b f eps (xn - ((f xn)/(i))) k (p-1) i


--pravi niz vrednosti izvoda funkcije f
niz_izvoda::[Double]->[Double]->[Double]->[Double]
niz_izvoda [] _ _ = []
niz_izvoda x f x0 = (D.izvod (head x) x0 f):(niz_izvoda (tail x) f x0)
