$w = 10

def fun(x, y)
	$w = 100
	x + y
end

z = fun(8, 15)
puts $w
puts z
