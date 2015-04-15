import Data.Maybe
import Data.List
main=interact$unlines.w.lines
p=partition;u='X';t="XX";n=Just;f=Nothing
w b=head$mapMaybe($b)[h,v,d,e,n]
h[]=f
h(l:m)=maybe(fmap(l:)$h m)(n.(:m))$i l
i l|(t,"_")==p(==u)l=n"XXX"|0<1=f
v[a:x,b:y,c:z]|(t,"_")==p(==u)[a,b,c]=n$map(u:)[x,y,z]|0<1=zipWith(:)[a,b,c]`fmap`v[x,y,z]
v _ = f
d[a:x,j:b:y,k:l:[c]]|(t,"_")==p(==u)[a,b,c]=n[u:x,j:u:y,k:l:"X"]|0<1=f
e=fmap reverse.d.reverse
