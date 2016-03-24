$n = 0

def add(x, y)
	$n += 1
	x + y + $n
end

puts add(3, 2) + add(1, 1) * add(2, 1)
