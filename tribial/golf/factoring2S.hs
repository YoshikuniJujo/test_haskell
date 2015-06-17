(!)=div
d=fromIntegral
main=interact$unwords.map show.f.map read.words
f[p,q,r]=[p!i!k,i,j,k,l]where z=round$(d q^2-4*d p*d r)**0.5;[b,c,e]=[2*p,q+z,q-z];s=signum p;[g,h]=[gcd c b*s,gcd e b*s];[v,w,x,y]=[b!g,c!g,b!h,e!h];[i,j,k,l]=if(x,y)>(v,w)then[x,y,v,w]else[v,w,x,y]
