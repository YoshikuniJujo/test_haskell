import Data.STRef
import Control.Monad.ST

some :: Int -> Int
some n = runST $ do
	r <- newSTRef n
	return 8
