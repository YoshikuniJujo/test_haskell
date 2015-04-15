main=interact$reverse.s.sum.map(foldl((+).(*2))0.map(read.(:""))).lines
s 0=""
s n=show(n`mod`2)++s(n`div`2)
