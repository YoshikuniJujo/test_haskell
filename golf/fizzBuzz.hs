import Data.List
import Control.Applicative
import Control.Monad
main=forM_[1..100]$putStrLn.(($) <$> ((map(foldr1(.))$transpose$zipWith(\n m->cycle$const m:replicate(n-1)id)[15,5,3]["Fizz Buzz","Buzz","Fizz"])!!)<*>show)
