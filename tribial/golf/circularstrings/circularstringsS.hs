main=interact$unlines.map(b.c.words).tail.lines
b True="true"
b _="false"
c[s,t]=s`elem`t#t
[]#_=[]
(_:d)#a@(x:s)=a:d#(s++[x])
