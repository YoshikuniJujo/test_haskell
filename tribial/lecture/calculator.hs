mplus :: Int -> Int -> ((), Int)
mplus x m = ((), x + m)

mrecall :: () -> Int -> (Int, Int)
mrecall _ m = (m, m)

arrC :: (a -> b) -> a -> Int -> (b, Int)
arrC f x m = (f x, m)

type Calc a b = a -> Int -> (b, Int)

pipeC :: Calc a b -> Calc b c -> Calc a c
(f `pipeC` g) x m = let (x', m') = f x m in g x' m'

example :: Calc () Int
example =
	arrC (const 3) `pipeC`
	arrC (* 4) `pipeC`
	mplus `pipeC`
	arrC (const 2) `pipeC`
	arrC (* 5) `pipeC`
	mplus `pipeC`
	mrecall `pipeC`
	arrC (* 7)

type State b = Int -> (b, Int)

bindC :: State a -> (a -> State b) -> State b
(f `bindC` g) m = let (x, m') = f m in g x m'

retC :: b -> State b
retC x m = (x, m)

example' :: State Int
example' =
	retC 3 `bindC`
	(retC . (* 4)) `bindC`
	mplus `bindC`
	const (retC 2) `bindC`
	(retC . (* 5)) `bindC`
	mplus `bindC`
	mrecall `bindC`
	(retC . (* 7))

example'' :: State Int
example'' =
	retC 3 `bindC` \x ->
	retC (x * 4) `bindC` \y ->
	mplus y `bindC` \_ ->
	retC 2 `bindC` \z ->
	retC (z * 5) `bindC` \w ->
	mplus w `bindC` \_ ->
	mrecall () `bindC` \v ->
	retC (v * 7)
