hello x
	| Just y <- x, odd y = "hoge"
	| otherwise = "hige"
