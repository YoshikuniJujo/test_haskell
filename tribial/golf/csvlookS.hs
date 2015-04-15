main=interact$unlines.f.o r.lines
o=map
z=length
y=concatMap
(#)=zipWith
(?)=replicate
r""=[""]
r(',':s)="":r s
r(c:s)=let h:t=r s in(c:h):t
l[s]=o z s
l(s:t)=max#o z s$l t
n m s="| "++init(y((' ':).(++" |"))$a#m$s)++" |"
a q t=t++(q-z t)?' '
p m="|-"++init(y((++"+").(?'-').(+2))m)++"-|"
f a@(s:t)=let m=l a in p m:n m s:p m:o(n m)t++[p m]
