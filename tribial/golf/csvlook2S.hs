main=interact$unlines.f.(((i?).r)?).lines
(#)=replicate;e=length;c=concatMap;(?)=map;z=zipWith
r""=[""]
r(',':s)="":r s
r(c:s)=let h:t=r s in(c:h):t
g[x]=e?x
g(x:y)=z max(e?x)$g y
l m s="| "++init(c((' ':).(++" |"))$z a m s)++" |"
a n s=s++(n-e s)#' '
i('"':s)=q s
i s=s
q('"':'"':s)='"':q s
q('"':_)=""
q(c:d)=c:q d
p m="|-"++init(c((++"+").(#'-').(+2))m)++"-|"
f b@(s:t)=p m:l m s:p m:(l m)?t++[p m]where m=g b
