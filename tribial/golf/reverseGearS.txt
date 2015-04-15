main=interact$show.r.sum.map(r.read).words
r(-30)= -13
r n=(*signum n).f.reverse.t$abs n
t 0=[]
t n=n`mod`10:t(n`div`10)
f[]=0
f(n:ns)=n+10*f ns
