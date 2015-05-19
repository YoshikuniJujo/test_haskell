def sum(data)
	s = 0
	data.each do |x| s += x end
	s
end

v = [3, 8, 2, 6, 1, 7, 9]
puts sum(v)
