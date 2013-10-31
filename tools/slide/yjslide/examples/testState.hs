type StateInt a = Int -> (a, Int)

memoryClear :: StateInt ()
memoryClear _ = ((), 0)

memoryPlus :: Int -> StateInt ()
memoryPlus x m = ((), m + x)

memoryMinus :: Int -> StateInt ()
memoryMinus x m = ((), m - x)

memoryRecall :: StateInt Int
memoryRecall m = (m, m)

liftNum :: Int -> StateInt Int
liftNum x m = (x, m)

(>>>=) :: StateInt a -> (a -> StateInt b) -> StateInt b
c >>>= f = \m -> let (x, m') = c m in f x m'

runCalc :: StateInt a -> a
runCalc = fst . ($ 0)

calc :: StateInt Int
calc =	memoryPlus 3 >>>= \_ ->
	memoryPlus 2 >>>= \_ ->
	memoryRecall >>>= \x ->
	memoryClear >>>= \_ ->
	memoryPlus 11 >>>= \_ ->
	memoryMinus 3 >>>= \_ ->
	memoryRecall >>>= \y ->
	liftNum (x * y)
