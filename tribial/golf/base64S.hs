main=interact$unlines.map(s.h).lines
(#)=div
(?)=mod
o=fromEnum
t=['A'..'Z']++['a'..'z']++['0'..'9']++"+/"
i c|o c<58=o c-48|1>0=o c-55
h""=[]
h(a:b:c)=i a*16+i b:h c
s[]=""
s[x]=map(t!!)[x#4,x?4*16]++"=="
s[x,y]=map(t!!)[x#4,x?4*16+y#16,y?16*4]++"="
s(x:y:z:n)=map(t!!)[x#4,x?4*16+y#16,y?16*4+z#64,z?64]++s n
