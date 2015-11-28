mul :: Integral n => n -> n -> n
mul = mulIter 0

mulIter :: Integral n => n -> n -> n -> n
mulIter s _ b | b < 1 = s
mulIter s a b
	| even b = mulIter s (a * 2) (b `div` 2)
	| otherwise = mulIter (s + a) a (b - 1)
