(!)=div
d=fromIntegral
main=interact$unwords.map show.f.map read.words
f[p,q,r]=[p!i!k,i,j,k,l]where z=round$(d q^2-4*d p*d r)**0.5;b=2*p;c=q+z;e=q-z;s=signum p;g=gcd c b*s;h=gcd e b*s;v=b!g;w=c!g;x=b!h;y=e!h;(i,j)=max(x,y)(v,w);(k,l)=min(x,y)(v,w)
