module Diferenciranje where
import Njutn_podeljene as N

napravi_f :: [Double] -> [Double] -> [Double]
napravi_f [xim,xi,xip] [fim,fi,fip] = [(fip-fim)/(xip-xim)]
napravi_f (xim:xi:xip:xs) (fim:fi:fip:fs) = ((fip-fim)/(xip-xim)):(napravi_f (xi:xip:xs) (fi:fip:fs))

  
izvod :: Double -> [Double] -> [Double] -> Double
izvod a x f = N.njutn a (drop 1 (take (length x -1) x)) (napravi_f x f) ((length x)-3)


