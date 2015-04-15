main=interact$unlines.d.lines
e='*'
p f g[x,y]=[f x,g y]
d["",t]=[map(\_->e)t,t]
d[s,""]=[s,map(\_->e)s]
d[a@(x:s),b@(y:t)]|x<y=p(x:)(e:)$d[s,b]|x>y=p(e:)(y:)$d[a,t]|0<1=p(x:)(y:)$d[s,t]
