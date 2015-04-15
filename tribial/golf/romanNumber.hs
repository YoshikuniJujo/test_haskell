main = interact$show.r

r :: String -> Int
r "" = 0
r ['I', 'V'] = 4
r ['I', 'X'] = 9
r ('I' : rs) = 1 + r rs
r ('V' : rs) = 5 + r rs
r ('X' : 'L' : rs) = 40 + r rs
r ('X' : 'C' : rs) = 90 + r rs
r ('X' : rs) = 10 + r rs
r ('L' : rs) = 50 + r rs
r ('C' : 'D' : rs) = 400 + r rs
r ('C' : 'M' : rs) = 900 + r rs
r ('C' : rs) = 100 + r rs
r ('D' : rs) = 500 + r rs
r ('M' : rs) = 1000 + r rs
