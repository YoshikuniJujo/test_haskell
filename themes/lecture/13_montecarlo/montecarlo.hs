import Control.Arrow
import System.Random

points :: Int -> [(Double, Double)]
points = uncurry zip . (randomRs (-1, 1) *** randomRs (-1, 1)) . split . mkStdGen

inCircle :: (Double, Double) -> Bool
inCircle (x, y) = x ^ 2 + y ^ 2 <= 1

inCirclePoints :: Int -> Int -> [(Double, Double)]
inCirclePoints g n = filter inCircle . take n $ points g

guessPi :: Int -> Int -> Double
guessPi g n = 4 * fromIntegral (length $ inCirclePoints g n) / fromIntegral n
