iteracija f a b fi eps = iter 0 a b fi eps
iter q a b fi eps | abs(q - (fi q))<eps = fi q
                  |otherwise = iter (fi q) a b fi eps
                  
                  
modifikacija f a b min_f max_f eps = iteracija f a b (\x->(x-(2/(min_f+max_f))*(f x)))
