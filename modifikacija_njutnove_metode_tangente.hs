napravi_f :: [Double] -> [Double] -> [Double]
napravi_f [xim,xi,xip] [fim,fi,fip] = [(fip-fim)/(xip-xim)]
napravi_f (xim:xi:xip:xs) (fim:fi:fip:fs) = ((fip-fim)/(xip-xim)):(napravi_f (xi:xip:xs) (fi:fip:fs))

podeljene_razlike :: [Double] -> [Double] -> Int -> Double
podeljene_razlike [x] [f] 0 = f

podeljene_razlike [x1, x2] [f1, f2] 1 = (f2 - f1)/(x2 - x1)

podeljene_razlike x f red
    | ((length f - 1) /= red)  || ((length x - 1) /= red) = error "Neispravni argumenti" 
    |otherwise = (podeljene_razlike (drop 1 x) (drop 1 f) (red-1) - podeljene_razlike (take ((length x)-1) x) (take ((length f)-1) f) (red-1))/ (x !! ((length x)-1) - x !! 0)
   
pom :: Double -> [Double] -> Int -> Double   
pom _ [x] _ = 1
pom a (x:xs) n = (a - x) * pom a xs (n-1)

njutn :: Double -> [Double] -> [Double] -> Int -> Double
njutn a x f n 
    | n == 0 = podeljene_razlike x f 0
    |otherwise = njutn a (take n x) (take n f) (n-1) + (podeljene_razlike x f n)*(pom a x (n-1))
  
izvod :: Double -> [Double] -> [Double] -> Double
izvod a x f = njutn a (drop 1 (take (length x -1) x)) (napravi_f x f) ((length x)-3)

njutn_resenje a b f eps k= njutn_pom a b f eps b k 0 0
njutn_pom a b f eps xn k 0 i | abs(((f xn)/(izvod xn ([a,a+((b-a)/10)..b]) (map f ([a,a+((b-a)/10)..b])))))<eps =xn
                             | otherwise = njutn_pom a b f eps (xn - ((f xn)/(izvod xn ([a,a+((b-a)/10)..b]) (map f ([a,a+((b-a)/10)..b]))))) k k (izvod xn ([a,a+((b-a)/10)..b]) (map f ([a,a+((b-a)/10)..b])))
njutn_pom a b f eps xn k p i | abs(((f xn)/(i)))<eps =xn
                             | otherwise = njutn_pom a b f eps (xn - ((f xn)/(i))) k (p-1) i

