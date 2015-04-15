main=interact$unlines.map(w.b).lines
(#)=(++)
b(_:s)=(g,v,takeWhile(/='[')r)where(t,_:r)=span(/=']')s;(g,v)=case span(/='=')t of(g,_:v)->(g,v);_->(t,"")
w("b","",x)="<strong>"#x#"</strong>"
w("i","",x)="<em>"#x#"</em>"
w("u","",x)="<span class=u>"#x#"</span>"
w("url",u,x)="<a href=\""#u#"\">"#x#"</a>"
w("img",u,x)="<img src=\""#u#"\" alt=\""#x#"\">"
