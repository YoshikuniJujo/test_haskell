main=interact$ \(n:s)->filter(`notElem`t(reverse v)(read[n])(last s)(cycle v))s
v=['a'..'z']
t p n r(c:s)|r==c=r:([p,s]>>=take n)|1>0=t(c:p)n r s
