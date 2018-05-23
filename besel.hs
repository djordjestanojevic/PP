import Gaus1 as G1
import Gaus2 as G2


--funkcija izracunava vrednost beselovog polinoma koriscenjem funkcija iz Gaus1.hs i Gaus2.hs koje implementiraju gausov interpolacioni polinom unapred odnosno unazad
besel:: Double->[Double]->[Double]->Int->Double
besel a x f n = ((G1.gaus a x f n)+(G2.gaus a x f n))/2
