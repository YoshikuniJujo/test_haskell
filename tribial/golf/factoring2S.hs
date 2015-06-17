main=interact$unwords.map show.f.map read.words
f[p,q,r]=[p`div`i`div`k,i,j,k,l]where z=round$(fromIntegral q^2-4*fromIntegral p*fromIntegral r)**0.5;[b,c,e]=[2*p,q+z,q-z];[g,h]=[gcd c b*signum p,gcd e b*signum p];[v,w,x,y]=[b`div`g,c`div`g,b`div`h,e`div`h];[i,j,k,l]=if(x,y)>(v,w)then[x,y,v,w]else[v,w,x,y]
