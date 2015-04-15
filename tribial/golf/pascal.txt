main=interact$unlines.s.(`take`p).read
w=show
l=length
z=zipWith
p=[1]:map(\l@(_:t)->1:z(+)l t++[1])p
s q=map(unwords.z t(map(l.w)$last q))q
t n x=replicate(n-l(w x))' '++w x
