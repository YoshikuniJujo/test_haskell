def myif(b, t, e)
	if b then t else e end
end

puts(if true then 88 else 95 / 0 end)

puts(myif(true, 88, 95 / 0))
