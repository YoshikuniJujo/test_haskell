import Numeric
t=init
m=map
main=interact$unlines.(\(x:s)->t x:s).m(t.unwords).w.m((\[r,n]->(read r,n)).words).lines
w r=("\\   ":n):zipWith(:)(m t n)(m(m i)$m((\x->m((/x).fst)r).fst)r)where n=m((++"  ").snd)r
i 1="1    "
i x=showFFloat(Just 2)x" "
