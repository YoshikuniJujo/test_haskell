main=interact$unwords.map show.f.map read.words
g=abs;v=reverse;y=signum
f n@[s,t,u]=[y s*a,b,c,d,e]where a=s`gcd`t`gcd`u;[p,q,r]=map(`quot`(a*y s))n;w=v[-g p..g p];x=v[-g r..g r];(b,c,d,e):_=[(b,c,d,e)|b<-w,c<-x,d<-w,e<-x,b*d==p,b*e+c*d==q,c*e==r]
