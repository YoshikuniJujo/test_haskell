{-

do	...
	(x, y) <- do
		x <- exp1
		y <- exp2
		return (x, y)
	...
	return result

	/ \
	| |
	\ /

do	x <- exp1
	y <- exp2
	return result

-}

{-

...

	(	exp1 >>= \x ->
		exp2 >>= \y ->
		return (x, y) ) >>= \(x, y) ->
	return result

	exp1 >>= \x ->
	exp2 >>= \y ->
	return result

-}

{-

右結合と左結合

(exp1 >>= \x -> exp2) >>= \y -> exp3
exp1 >>= \x -> (exp2 >>= \y -> exp3)

-}
