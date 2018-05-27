module Prosta_iteracija where

--pronalazi nulu eksplicitno zadate funkcije f koriscenjem kontrakcije koja je jedan od argumenata
iteracija:: (Double->Double)->Double->Double->(Double->Double)->Double->Double
iteracija f a b fi eps = iter 0 a b fi eps

iter:: Double->Double->Double->(Double->Double)->Double->Double
iter q a b fi eps | abs(q - (fi q))<eps = fi q
                  |otherwise = iter (fi q) a b fi eps
                  
modifikacija::(Double->Double)->Double->Double->Double->Double->Double->Double
modifikacija f a b min_f max_f eps = iteracija f a b (\x->(x-(2/(min_f+max_f))*(f x))) eps