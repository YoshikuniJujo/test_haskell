import Control.Arrow
import System.Random

inCirclePoints :: Int -> Int -> [(Double, Double)]
inCirclePoints g n = filter ((<= 1) . uncurry (+) . ((^ 2) *** (^ 2)))
	. take n . uncurry zip
	. (randomRs (-1, 1) *** randomRs (-1, 1)) . split $ mkStdGen g

guessPi :: Int -> Int -> Double
guessPi g n = 4 * fromIntegral (length $ inCirclePoints g n) / fromIntegral n
