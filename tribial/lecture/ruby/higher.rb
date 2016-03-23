def five
	i = 1
	while (i < 6)
		yield i
		i += 1
	end
end

five { |i| puts i }

five { |i| puts "*" * i }
