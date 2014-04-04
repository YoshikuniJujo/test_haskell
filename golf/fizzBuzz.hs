import Data.List
import Control.Applicative
main=mapM(putStrLn.(($)<$>((map(foldr1(.))$transpose$zipWith(\n m->cycle$const m:replicate(n-1)id)[15,5,3]["Fizz Buzz","Buzz","Fizz"])!!)<*>show))[1..100]
