japanese = [10000, 5000, 2000, 1000, 500, 100, 50, 10, 5, 1]

change _ a | a < 0 = 0
change _ 0 = 1
change [] _ = 0
change ma@(m : ms) a = change ma (a - m) + change ms a
