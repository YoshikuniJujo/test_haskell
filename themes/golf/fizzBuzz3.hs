import Data.List
main=mapM putStrLn$zipWith($)(map(foldr1(.))$transpose$zipWith(\n m->cycle$replicate(n-1)id++[\_->m])[15,5,3]["Fizz Buzz","Buzz","Fizz"])$map show[1..100]
