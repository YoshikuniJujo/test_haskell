import Data.Char
main=getLine>>=b 0[].(,)""
b _ m(_,"")=return m
b p m(r,'>':d)=b(succ p)m('>':r,d)
b p m(r,'<':d)=b(pred p)m('<':r,d)
b p m(r,'+':d)=b p(o succ p m)('+':r,d)
b p m(r,'-':d)=b p(o pred p m)('-':r,d)
b p m(r,'.':d)=putChar(chr$m!!p)>>b p m('.':r,d)
b p m(r,',':d)=do c<-getChar;b p(o(const$ord c)p m)(',':r,d)
b p m(r,'[':d)|length m<p+1||m!!p==0=b p m.k 1$('[':r,d)|0<1=b p m('[':r,d)
b p m(r,']':d)|m!!p/=0=b p m.t 1$(r,']':d)|0<1=b p m(']':r,d)
b p m(r,_:d)=b p m(r,d)
o f p m|length m<p+1=o f p$m++replicate(p+1-length m)0|0<1=take p m++[f$m!!p]++drop(p+1)m
k n p|n<1=p
k n(r,'[':s)=k(n+1)('[':r,s)
k n(r,']':s)=k(n-1)(']':r,s)
k n(r,c:s)=k n(c:r,s)
t n p|n<1=p
t n(']':r,s)=t(n+1)(r,']':s)
t n('[':r,s)=t(n-1)(r,'[':s)
t n(c:r,s)=t n(r,c:s)
