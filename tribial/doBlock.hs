filterA = [3]
filterB = [2]
output = []

funA n = if not (elem n filterA)
	then do	filterA ++ [3 * n + 2]
		filterB ++ [3 * n + 2]
		output ++ [3 * n + 2]
	else output

funB n = if not (elem n filterA)
	then let
		filterA' = filterA ++ [3 * n + 2]
		filterB' = filterB ++ [3 * n + 2]
		output' = output ++ [3 * n + 2] in
		(filterA', filterB', output')
	else (filterA, filterB, output)
