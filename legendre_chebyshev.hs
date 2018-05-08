legendre x 0 = 1
legendre x 1 = x
legendre x n = (1/(n))*((2*n-1)*x*(legendre x (n-1))-(n-1)*(legendre x (n-2)))

nule_chebyshev a b n k |n==k = []
                       |otherwise= ((((b-a)/2)*(cos((2*k+1)*3.141592653/(2*n))))+((b+a)/2)):(nule_chebyshev a b n (k+1))

chebyshev x 0 = 1
chebyshev x 1 = x
chebyshev x n = 2*x*(chebyshev x (n-1))-(chebyshev x (n-2))
