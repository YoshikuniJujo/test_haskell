def function(x)
	x + 10
end

def procedure
	$ret = $arg + 10
end

puts(function(5))

$arg = 5
procedure()
puts($ret)
