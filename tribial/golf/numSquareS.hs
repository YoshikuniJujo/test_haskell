main=interact$show.c.read
c n|any(==0)d=1|any(`elem`s)d=2|any(`elem`takeWhile(<=n)(o 0[2..]$map(\x->map(\y->x^2+y^2)[x..])[1..]))d=3|True=4where s=takeWhile(<=n)$map(^2)[1..];d=map(n-)s
t 0_ s=(False,s)
t n y(a@(x:u):s)|x==y=(True,u:snd(t(n-1)y s))|True=(\(f,r)->(f,a:r))$t(n-1)y s
o n(y:u)s|(x:_)<-s!!n,x==y=y:o(n+1)u r|f=y:o n u r|True=o n u r where(f,r)=t(n+1)y s
