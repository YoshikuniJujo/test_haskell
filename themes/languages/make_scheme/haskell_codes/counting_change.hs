import Data.Maybe
import qualified Data.Map as M

countChangeRaw :: [Int] -> Int -> Int
countChangeRaw _ 0 = 1
countChangeRaw _ a | a < 0 = 0
countChangeRaw [] _ = 0
countChangeRaw da@(d : ds) a = countChangeRaw ds a + countChangeRaw da (a - d)

ccr :: Int -> Int
ccr = countChangeRaw [1, 5, 10, 25, 50]

countChangeMemo :: [Int] -> Int -> M.Map ([Int], Int) Int
countChangeMemo da 0 = M.fromList [((da, 0), 1)]
countChangeMemo da a | a < 0 = M.fromList [((da, a), 0)]
countChangeMemo [] a = M.fromList [(([], a), 0)]
countChangeMemo da@(d : ds) a =
	M.insert (da, a)
		(fromJust (M.lookup (ds, a) m) + fromJust (M.lookup (da, a') m))
		m
	where
	a' = a - d
	m = countChangeMemo ds a `M.union` countChangeMemo da a'

countChange :: [Int] -> Int -> Int
countChange da a = fromJust . M.lookup (da, a) $ countChangeMemo da a

ccm :: Int -> Int
ccm = countChange [1, 5, 10, 25, 50]
