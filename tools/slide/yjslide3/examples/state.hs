type State a = Int -> (a, Int)

-- mplus :: Int -> State ()
mplus :: Int -> Int -> ((), Int)
mplus n = \s -> ((), s + n)

-- mrecall :: () -> State Int
mrecall :: () -> Int -> (Int, Int)
mrecall _ = \s -> (s, s)

pipeS :: (a -> State b) -> (b -> State c) -> (a -> State c)
pipeS f g = \x -> \s -> let (x', s') = (f x) s in g x' s'

arrS :: (a -> b) -> (a -> State b)
arrS f = \x -> \s -> (f x, s)

example :: () -> State Int
example =
	(\_ -> mplus (3 * 4)) `pipeS`
	(\_ -> mplus (2 * 5)) `pipeS`
	mrecall `pipeS`
	arrS (* 7)
