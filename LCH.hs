module LCH where
legendre :: Double -> Int -> Double
legendre x 0 = 1
legendre x 1 = x
legendre x n = (1/fromIntegral(n))*(fromIntegral(2*n-1)*x*(legendre x (n-1))-(fromIntegral(n-1)*(legendre x (n-2))))


nule_chebyshev :: Double -> Double -> Int -> Int -> [Double]
nule_chebyshev a b n k |n==k = []
                       |otherwise= ((((b-a)/2)*(cos((2*(fromIntegral(k+1)))*3.141592653/(fromIntegral(2*n))))+((b+a)/2))):(nule_chebyshev a b n (k+1))

                       
chebyshev :: Double -> Int -> Double                       
chebyshev x 0 = 1
chebyshev x 1 = x
chebyshev x n = 2*x*(chebyshev x (n-1))-(chebyshev x (n-2))




