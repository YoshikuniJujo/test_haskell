import Data.List
import Control.Applicative
import Control.Monad

converts n m = cycle $ const m : replicate (n - 1) id

c = map (foldr1 (.)) $ transpose $
	zipWith converts [15, 5, 3] ["Fizz Buzz", "Buzz", "Fizz"]

main = forM_ [1 .. 20] $ putStrLn . (($) <$> (c !!) <*> show)
