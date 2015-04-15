import Data.List
import Data.Ratio
main=interact$show.numerator.c.a.map e.lines
data R=R(Ratio Int)|R:>R|R:|R|N
c(R r)=r
c(r:>s)=c r+c s
c(r:|s)=c r*c s/(c r+c s)
c N=0
data W=S|E|U|D|B|T Int deriving Eq
e""=[]
e('@':'-':s)=S:e s
e('-':'@':s)=E:e s
e('/':'-':s)=e s
e('\\':'-':s)=e s
e('/':s)=U:e s
e('\\':s)=D:e s
e('<':s)=B:e s
e('|':s)=let(n,_:r)=span(/='|')s in T(read n):e r
e(_:s)=e s
a w=fst$f Nothing(i,w)where Just i=findIndex(elem S)w
f b(i,w)=case p i w of(S,s)->f b(i,s);(E,s)->(N,(i,s));(T r,s)->let(n,(j,t))=f b(i,s)in(R(r%1):>n,(j,t));(B,s)->let(u,(_,t))=f(Just i)(i-1,s);(d,(_,v))=f(Just i)(i+1,t);(n,(j,w))=f b(i,v)in(u:|d:>n,(j,w));(D,s)->if i+1==(\(Just x)->x)b then(N,(i+1,s))else f b(i+1,s);(U,s)->if i-1==(\(Just x)->x)b then(N,(i-1,s))else f b(i-1,s)
p 0((w:s):t)=(w,s:t)
p n(w:s)=(v,w:t)where(v,t)=p(n-1)s
