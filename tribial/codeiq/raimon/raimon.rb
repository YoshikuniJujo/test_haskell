def rotate(ss)
	is = []
	ss[0].length.times{|i| is.unshift i}
	is.map{|i| ss.map{|l| l[i]}.join}
end

def spiral(n)
	if n == 0 then return [""] end
	rotate(spiral(n - 1)).map{|l| l + ' '}.unshift('#' * n)
end

def zipWith(x, y, &b) x.zip(y).map(&b) end

def raimon(n)
	zipWith(spiral(n), rotate(rotate(spiral(n)))) { |l1, l2| l1 + '#' + l2 }
end

puts raimon(gets.to_i * 4 + 1)
