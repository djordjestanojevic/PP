import Gaus1 as G1
import Gaus2 as G2

besel a x f n = ((G1.gaus a x f n)+(G2.gaus a x f n))/2
