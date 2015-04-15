import Data.Char
b=0<1
(#)=(++)
f=dropWhile
type S=String
main=interact$w.p.r.lines
data M=H S|I S|P[M]|R S|E S|S S|U[M]|L M|O[M]|N S|Z|S:#S|S:&S|B
r[]=[]
r("":t)=r t
r(('#':' ':s):t)=H(head$words s):r t
r(('*':'*':s):t)=S(takeWhile(/='*')s):r t
r(('*':s):t)=E(init s):r t
r((' ':'-':s):t)=L(head$r[f(==' ')s]):r t
r(('-':_):t)=Z:r t
r(s@(i:_):t)|isDigit i=N(f isSpace.tail$f(/='.')s):r t
r(h:('-':_):t)=I h:r t
r(('[':s):t)=let(x,u)=a s in x:#u:r t
r((_:'[':s):t)=let(x,u)=a s in x:&u:r t
r(w:t)=R w:r t
a s=case span(/=']')s of(x,']':'(':u)->(x,init u)
q(R _)=b
q(S _)=b
q(E _)=b
q(_:#_)=b
q(_:&_)=b
q _=1<0
v(L _)=b
v _=1<0
y(N _)=b
y _=1<0
p[]=[]
p(h@(H _):s)=h:p s
p(i@(I _):s)=i : p s
p(Z:s)=Z:p s
p s@(L _:_)=let(r,t)=span v s in U r:p t
p s@(N _:_)=let(r,t)=span y s in O r:p t
p s=let(r,t)=span q s in P (k r):p t
k[x]=[x]
k(x:s)=x:B:k s
w[]=""
w(H h:m)=j"h1"h#w m
w(I i:m)=j"h2"i#w m
w(P m:t)=j"p"(w m)#w t
w(S t:m)=j"strong"t#w m
w(E e:m)=j"em"e#w m
w(U m:t)=j"ul"(w m)#w t
w(L m:t)=j"li"(w[m])#w t
w(O m:t)=j"ol"(w m)#w t
w(N m:t)=j"li"m#w t
w(Z:t)="<hr>"#w t
w(B:t)="<br>"#w t
w(R s:t)=s#w t
w(x:#u:t)="<a href=\""#u#"\">"#x#"</a>"#w t
w(x:&u:t)="<img src=\""#u#"\" alt=\""#x#"\">"#w t
j s t="<"#s#">"#t#"</"#s#">"
