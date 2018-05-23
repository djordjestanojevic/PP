import Diferenciranje as D
--pronalazi nulu eksplicitno zadate funkcije f na intervalu a b sa tacnoscu eps
njutn_resenje:: Double->Double->(Double->Double)->Double->Double
njutn_resenje a b f eps = njutn_pom a b f eps b

--funkcija sa zadatim pocetnim cvorom
njutn_pom:: Double->Double->(Double->Double)->Double->Double->Double
njutn_pom a b f eps xn | abs(r)<eps = xn-r
                       | otherwise = njutn_pom a b f eps (xn - r)
    where r= ((f xn)/(D.izvod xn ([a,a+((b-a)/10)..b]) (map f ([a,a+((b-a)/10)..b]))))

--modifikacija njutnove metode tangente
modifikacija:: Double->Double->(Double->Double)->Double->Int->Double
modifikacija a b f eps k = mod_pom a b f eps b k 0 0


mod_pom::  Double->Double->(Double->Double)->Double->Int->Int->Double->Double
mod_pom a b f eps xn k 0 i | abs((f xn)/(iz))<eps =xn
                             | otherwise = mod_pom a b f eps (xn - ((f xn)/(iz))) k k (iz)
    where iz = D.izvod xn ([a,a+((b-a)/10)..b]) (map f ([a,a+((b-a)/10)..b]))
mod_pom a b f eps xn k p i | abs(((f xn)/(i)))<eps =xn
                             | otherwise = mod_pom a b f eps (xn - ((f xn)/(i))) k (p-1) i

