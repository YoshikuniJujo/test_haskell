fun2 = replicate

fun1 = (fun2 <$> length <*> reverse) . tail

fun1' l = fun2 (length l') (reverse l')
	where
	l' = tail l

fun1'' l = let l' = tail l in
	fun2 (length l') (reverse l')
