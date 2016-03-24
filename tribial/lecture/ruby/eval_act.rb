$n = 0

def run(act)
	act.each do |a|
		if a[0] == :add
			$n += a[1]
		elsif a[0] == :sub
			$n -= a[1]
		end
	end
end

id = []

def add(a, x)
	a.push [:add, x]
end

def sub(a, x)
	a.push [:sub, x]
end

action = add(sub(add(id, 15), 8), 5)

run(action)

puts $n
