type Calc a b = a -> Int -> (b, Int)

mplus :: Int -> Int -> ((), Int)
mplus n m = ((), n + m)

mrecall :: () -> Int -> (Int, Int)
mrecall _ m = (m, m)

arrC :: (a -> b) -> a -> Int -> (b, Int)
arrC f x m = (f x, m)

pipeC :: (a -> Int -> (b, Int)) -> (b -> Int -> (c, Int)) -> (a -> Int -> (c, Int))
f `pipeC` g = \x m -> let (x', m') = f x m in g x' m'

example :: () -> Int -> (Int, Int)
example =
	arrC (const 3) `pipeC`
	arrC (*) `pipeC`
	arrC ($ 4) `pipeC`
	mplus `pipeC`
	arrC (const 2) `pipeC`
	arrC (*) `pipeC`
	arrC ($ 5) `pipeC`
	mplus `pipeC`
	mrecall `pipeC`
	arrC (* 7)
