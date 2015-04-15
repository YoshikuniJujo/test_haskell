import Data.List
b=map
main=interact$unlines.d.s(b(b snd).groupBy(n(==)fst).nub.sort).t 0(0,0).b(\(q:n)->(q,read n)).words
s f(x,y)=(x,f y)
n v w x y=w x`v`w y
t m i[]=(m+1,[i])
t m i((o,0):q)=t m i q
t m i((o,n):q)=(i:)`s`t(max m x)r((o,n-1):q)where r@(_,x)=a o i
a 'U'(y,x)=(y-1,x)
a 'D'(y,x)=(y+1,x)
a 'L'(y,x)=(y,x-1)
a 'R'(y,x)=(y,x+1)
d(m,p)=b(l m)p
l m[]=replicate m '_'
l m(0:xs)='#':l(m-1)(b pred xs)
l m xs='_':l(m-1)(b pred xs)
