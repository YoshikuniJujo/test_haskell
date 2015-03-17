import Data.List
import Data.Ratio
main=interact$show.numerator.c.e.lines
data R=R(Ratio Int)|R:>R|R:|R|N
c(R r)=r
c(s:>t)=c s+c t
c(s:|t)=c s*c t/(c s+c t)
c N=0
l s i x=take i s++[x]++drop(i+1)s
e s=fst$g Nothing i s where Just i=findIndex(elem '@')s
g b i x=let a w=g b i(l x i w)in case x!!i of '/':'-':w->a w;'\\':'-':w->a w;'-':'@':w->(N,(i,l x i w));'|':w->let(n,_:t)=span(/='|')w;(r,s)=g b i(l x i t)in(R(read n%1):>r,s);'<':w->let(u,(_,v))=g(Just i)(i-1)(l x i w);(d,(_,y))=g(Just i)(i+1)v;(n,(_,z))=g b i y in(u:|d:>n,(i,z));'\\':w->let Just s=b in if i+1==s then(N,(i+1,l x i w))else g b(i+1)(l x i w);'/':w->let Just s=b in if i-1==s then(N,(i-1,l x i w))else g b(i-1)(l x i w);_:w->a w
