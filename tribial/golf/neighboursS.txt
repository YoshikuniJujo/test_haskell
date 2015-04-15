import Data.List
main=interact$show.sum.nub.g t.sort.map read.words
t=True
g b[p]|b=[p]|t=[]
g b(p:a@(n:_))|(p-n)^2==1=g(1<0)a|b=p:g t a|t=g t a
