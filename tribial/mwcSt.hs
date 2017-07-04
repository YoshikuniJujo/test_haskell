import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Data.List
import Data.Word
import Data.Vector (fromList, Vector)
import System.Random.MWC

getInt :: [Word32] -> Word
getInt v = runST $ (>>= uniform) . initialize $ fromList v

some :: [Word]
some = take 1000 . map getInt $ unfoldr (\ns -> Just (ns, 1 : map succ ns)) []

other :: [Int]
other = findIndices not $ zipWith (==) some (tail some)

getWords :: [Word32] -> [Word8]
getWords v = runST $ do
	g <- initialize $ fromList v
	rec g 100
	{-
	x <- uniform g
	y <- uniform g
	return [x, y]
	-}
--	fixST $ \ns -> (: ns) <$> uniform g

rec :: (Gen (PrimState (ST s))) -> Int -> ST s [Word8]
rec g 0 = return []
rec g n = do
	x <- uniform g
	(x :) <$> rec g (pred n)
