import Data.List
import Numeric.Natural

f :: Natural -> Natural -> Natural
f i n = iterate (\n -> n * (n + 1)) n `genericIndex` i

fact :: Natural -> [Natural]
fact 0 = [0]
fact 1 = []
fact n = k : fact (n `div` k) where k = head $ filter ((== 0) . (n `mod`)) [2 .. n]

check :: Natural -> Natural -> Bool
check i n = genericLength (nub . fact $ f i n) > i
