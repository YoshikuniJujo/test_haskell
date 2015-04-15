main=interact$show.(\x->x-1000).r 1000.map t
t 'I'=1
t 'V'=5
t 'X'=10
t 'L'=50
t 'C'=100
t 'D'=500
t 'M'=1000
r p[]=p
r p(n:s)|p<n=r n s-p|0<1=p+r n s
