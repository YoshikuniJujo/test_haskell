import Data.Char
l=length
main=getLine>>=b 0[].(,)""
b _ m(_,"")=return()
b p m(r,a:d)=case a of '>'->b(succ p)m(a:r,d);'<'->b(pred p)m(a:r,d);'+'->b p(o succ p m)(a:r,d);'-'->b p(o pred p m)(a:r,d);'.'->putChar(chr$m!!p)>>b p m(a:r,d);','->do{c<-getChar;b p(o(const$ord c)p m)(a:r,d)};'['|l m<p+1||m!!p==0->b p m.k 1$(a:r,d)|0<1->b p m(a:r,d); ']'|m!!p/=0->b p m.t 1$(r,a:d)|0<1->b p m(a:r,d);
o f p m|l m<p+1=o f p$m++replicate(p+1-l m)0|0<1=take p m++[f$m!!p]++drop(p+1)m
k n p|n<1=p
k n(r,'[':s)=k(n+1)('[':r,s)
k n(r,']':s)=k(n-1)(']':r,s)
k n(r,c:s)=k n(c:r,s)
t n p|n<1=p
t n(']':r,s)=t(n+1)(r,']':s)
t n('[':r,s)=t(n-1)(r,'[':s)
t n(c:r,s)=t n(r,c:s)
