import Data.List
main=interact$s.c.lines
f=findIndices
e=(`notElem`" +-|")
v=filter
p=concatMap
t=takeWhile
d=drop
n=length
w=(/='+')
s r@[_] = r
s _ = "None"
a=all
c m=map fst.v(h.(l m))$zip(p(v e.(m!!))y).zip(cycle y)$p(f e.(m!!))y where y=f(any e)m
l m(c,(y,x))=([o,t w.d(x+1)$m!!(y+n a+1)],[a,t w.d(y+1)$map(!!(x+n o+1))m])where o=t w.d(x+1)$m!!y;a=t w.d(y+1)$map(!!x)m
h(r,w)=a(a(=='-'))r&&a(a(=='|'))w
