def sum(data)
	data.inject(0) {|s, x| s + x }
end

v = [3, 8, 2, 6, 1, 7, 9]
puts sum(v)
puts sum([])
